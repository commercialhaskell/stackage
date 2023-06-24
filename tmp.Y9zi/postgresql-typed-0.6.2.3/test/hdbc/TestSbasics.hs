module TestSbasics(tests) where
import Test.HUnit
import Data.List
import Database.HDBC
import TestUtils
import Control.Exception

openClosedb = sqlTestCase $ 
    do dbh <- connectDB
       disconnect dbh

multiFinish = dbTestCase (\dbh ->
    do sth <- prepare dbh "SELECT 1 + 1"
       sExecute sth []
       finish sth
       finish sth
       finish sth
                          )

runRawTest = dbTestCase (\dbh ->
    do runRaw dbh "CREATE TABLE valid1 (a int); CREATE TABLE valid2 (a int)"
       tables <- getTables dbh
       assertBool "valid1 table not created!" ("valid1" `elem` tables)
       assertBool "valid2 table not created!" ("valid2" `elem` tables)
                        )

runRawErrorTest = dbTestCase (\dbh ->
    let expected = "ERROR: syntax error at or near \"INVALID\""
    in do err <- (runRaw dbh "CREATE TABLE valid1 (a int); INVALID" >> return "No error") `catchSql`
                 (return . seErrorMsg)
          assertBool "Error message inappropriate" (expected `isPrefixOf` err)
          rollback dbh
          tables <- getTables dbh
          assertBool "valid1 table created!" (not $ "valid1" `elem` tables)
                             )


basicQueries = dbTestCase (\dbh ->
    do sth <- prepare dbh "SELECT 1 + 1"
       sExecute sth []
       sFetchRow sth >>= (assertEqual "row 1" (Just [Just "2"]))
       sFetchRow sth >>= (assertEqual "last row" Nothing)
                          )
    
createTable = dbTestCase (\dbh ->
    do sRun dbh "CREATE TABLE hdbctest1 (testname VARCHAR(20), testid INTEGER, testint INTEGER, testtext TEXT)" []
       commit dbh
                         )

dropTable = dbTestCase (\dbh ->
    do sRun dbh "DROP TABLE hdbctest1" []
       commit dbh
                       )

runReplace = dbTestCase (\dbh ->
    do sRun dbh "INSERT INTO hdbctest1 VALUES (?, ?, ?, ?)" r1
       sRun dbh "INSERT INTO hdbctest1 VALUES (?, ?, 2, ?)" r2
       commit dbh
       sth <- prepare dbh "SELECT * FROM hdbctest1 WHERE testname = 'runReplace' ORDER BY testid"
       sExecute sth []
       sFetchRow sth >>= (assertEqual "r1" (Just r1))
       sFetchRow sth >>= (assertEqual "r2" (Just [Just "runReplace", Just "2",
                                                 Just "2", Nothing]))
       sFetchRow sth >>= (assertEqual "lastrow" Nothing)
                       )
    where r1 = [Just "runReplace", Just "1", Just "1234", Just "testdata"]
          r2 = [Just "runReplace", Just "2", Nothing]

executeReplace = dbTestCase (\dbh ->
    do sth <- prepare dbh "INSERT INTO hdbctest1 VALUES ('executeReplace',?,?,?)"
       sExecute sth [Just "1", Just "1234", Just "Foo"]
       sExecute sth [Just "2", Nothing, Just "Bar"]
       commit dbh
       sth <- prepare dbh "SELECT * FROM hdbctest1 WHERE testname = ? ORDER BY testid"
       sExecute sth [Just "executeReplace"]
       sFetchRow sth >>= (assertEqual "r1" 
                         (Just $ map Just ["executeReplace", "1", "1234", 
                                           "Foo"]))
       sFetchRow sth >>= (assertEqual "r2"
                         (Just [Just "executeReplace", Just "2", Nothing,
                                Just "Bar"]))
       sFetchRow sth >>= (assertEqual "lastrow" Nothing)
                            )

testExecuteMany = dbTestCase (\dbh ->
    do sth <- prepare dbh "INSERT INTO hdbctest1 VALUES ('multi',?,?,?)"
       sExecuteMany sth rows
       commit dbh
       sth <- prepare dbh "SELECT testid, testint, testtext FROM hdbctest1 WHERE testname = 'multi'"
       sExecute sth []
       mapM_ (\r -> sFetchRow sth >>= (assertEqual "" (Just r))) rows
       sFetchRow sth >>= (assertEqual "lastrow" Nothing)
                          )
    where rows = [map Just ["1", "1234", "foo"],
                  map Just ["2", "1341", "bar"],
                  [Just "3", Nothing, Nothing]]

testsFetchAllRows = dbTestCase (\dbh ->
    do sth <- prepare dbh "INSERT INTO hdbctest1 VALUES ('sFetchAllRows', ?, NULL, NULL)"
       sExecuteMany sth rows
       commit dbh
       sth <- prepare dbh "SELECT testid FROM hdbctest1 WHERE testname = 'sFetchAllRows' ORDER BY testid"
       sExecute sth []
       results <- sFetchAllRows sth
       assertEqual "" rows results
                               )
    where rows = map (\x -> [Just . show $ x]) [1..9]

basicTransactions = dbTestCase (\dbh ->
    do assertBool "Connected database does not support transactions; skipping transaction test" (dbTransactionSupport dbh)
       sth <- prepare dbh "INSERT INTO hdbctest1 VALUES ('basicTransactions', ?, NULL, NULL)"
       sExecute sth [Just "0"]
       commit dbh
       qrysth <- prepare dbh "SELECT testid FROM hdbctest1 WHERE testname = 'basicTransactions' ORDER BY testid"
       sExecute qrysth []
       sFetchAllRows qrysth >>= (assertEqual "initial commit" [[Just "0"]])

       -- Now try a rollback
       sExecuteMany sth rows
       rollback dbh
       sExecute qrysth []
       sFetchAllRows qrysth >>= (assertEqual "rollback" [[Just "0"]])

       -- Now try another commit
       sExecuteMany sth rows
       commit dbh
       sExecute qrysth []
       sFetchAllRows qrysth >>= (assertEqual "final commit" ([Just "0"]:rows))
                               )
    where rows = map (\x -> [Just . show $ x]) [1..9]

testWithTransaction = dbTestCase (\dbh ->
    do assertBool "Connected database does not support transactions; skipping transaction test" (dbTransactionSupport dbh)
       sth <- prepare dbh "INSERT INTO hdbctest1 VALUES ('withTransaction', ?, NULL, NULL)"
       sExecute sth [Just "0"]
       commit dbh
       qrysth <- prepare dbh "SELECT testid FROM hdbctest1 WHERE testname = 'withTransaction' ORDER BY testid"
       sExecute qrysth []
       sFetchAllRows qrysth >>= (assertEqual "initial commit" [[Just "0"]])
       
       -- Let's try a rollback.
       catch (withTransaction dbh (\_ -> do sExecuteMany sth rows
                                            fail "Foo"))
             (\SomeException{} -> return ())
       sExecute qrysth []
       sFetchAllRows qrysth >>= (assertEqual "rollback" [[Just "0"]])

       -- And now a commit.
       withTransaction dbh (\_ -> sExecuteMany sth rows)
       sExecute qrysth []
       sFetchAllRows qrysth >>= (assertEqual "final commit" ([Just "0"]:rows))
                               )
    where rows = map (\x -> [Just . show $ x]) [1..9]
       
tests = TestList
        [
         TestLabel "openClosedb" openClosedb,
         TestLabel "multiFinish" multiFinish,
         TestLabel "runRawTest" runRawTest,
         TestLabel "runRawErrorTest" runRawErrorTest,
         TestLabel "basicQueries" basicQueries,
         TestLabel "createTable" createTable,
         TestLabel "runReplace" runReplace,
         TestLabel "executeReplace" executeReplace,
         TestLabel "executeMany" testExecuteMany,
         TestLabel "sFetchAllRows" testsFetchAllRows,
         TestLabel "basicTransactions" basicTransactions,
         TestLabel "withTransaction" testWithTransaction,
         TestLabel "dropTable" dropTable
         ]
