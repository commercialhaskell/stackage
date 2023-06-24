module Testbasics(tests) where
import Test.HUnit
import Database.HDBC
import TestUtils
import System.IO
import Control.Exception

openClosedb = sqlTestCase $ 
    do dbh <- connectDB
       disconnect dbh

multiFinish = dbTestCase (\dbh ->
    do sth <- prepare dbh "SELECT 1 + 1"
       r <- execute sth []
       assertEqual "basic count" 0 r
       finish sth
       finish sth
       finish sth
                          )

basicQueries = dbTestCase (\dbh ->
    do sth <- prepare dbh "SELECT 1 + 1"
       execute sth [] >>= (0 @=?)
       r <- fetchAllRows sth
       assertEqual "converted from" [["2"]] (map (map fromSql) r)
       assertEqual "int32 compare" [[SqlInt32 2]] r
       assertEqual "iToSql compare" [[iToSql 2]] r
       assertEqual "num compare" [[toSql (2::Int)]] r
       assertEqual "nToSql compare" [[nToSql (2::Int)]] r
       assertEqual "string compare" [[SqlString "2"]] r
                          )
    
createTable = dbTestCase (\dbh ->
    do run dbh "CREATE TABLE hdbctest1 (testname VARCHAR(20), testid INTEGER, testint INTEGER, testtext TEXT)" []
       commit dbh
                         )

dropTable = dbTestCase (\dbh ->
    do run dbh "DROP TABLE hdbctest1" []
       commit dbh
                       )

runReplace = dbTestCase (\dbh ->
    do r <- run dbh "INSERT INTO hdbctest1 VALUES (?, ?, ?, ?)" r1
       assertEqual "insert retval" 1 r
       run dbh "INSERT INTO hdbctest1 VALUES (?, ?, ?, ?)" r2
       commit dbh
       sth <- prepare dbh "SELECT * FROM hdbctest1 WHERE testname = 'runReplace' ORDER BY testid"
       rv2 <- execute sth []
       assertEqual "select retval" 0 rv2
       r <- fetchAllRows sth
       assertEqual "" [r1, r2] r
                       )
    where r1 = [toSql "runReplace", iToSql 1, iToSql 1234, SqlString "testdata"] 
          r2 = [toSql "runReplace", iToSql 2, iToSql 2, SqlNull]

executeReplace = dbTestCase (\dbh ->
    do sth <- prepare dbh "INSERT INTO hdbctest1 VALUES ('executeReplace',?,?,?)"
       execute sth [iToSql 1, iToSql 1234, toSql "Foo"]
       execute sth [SqlInt32 2, SqlNull, toSql "Bar"]
       commit dbh
       sth <- prepare dbh "SELECT * FROM hdbctest1 WHERE testname = ? ORDER BY testid"
       execute sth [SqlString "executeReplace"]
       r <- fetchAllRows sth
       assertEqual "result"
                   [[toSql "executeReplace", iToSql 1, toSql "1234",
                     toSql "Foo"],
                    [toSql "executeReplace", iToSql 2, SqlNull,
                     toSql "Bar"]]
                   r
                            )

testExecuteMany = dbTestCase (\dbh ->
    do sth <- prepare dbh "INSERT INTO hdbctest1 VALUES ('multi',?,?,?)"
       executeMany sth rows
       commit dbh
       sth <- prepare dbh "SELECT testid, testint, testtext FROM hdbctest1 WHERE testname = 'multi'"
       execute sth []
       r <- fetchAllRows sth
       assertEqual "" rows r
                          )
    where rows = [map toSql ["1", "1234", "foo"],
                  map toSql ["2", "1341", "bar"],
                  [toSql "3", SqlNull, SqlNull]]

testFetchAllRows = dbTestCase (\dbh ->
    do sth <- prepare dbh "INSERT INTO hdbctest1 VALUES ('sFetchAllRows', ?, NULL, NULL)"
       executeMany sth rows
       commit dbh
       sth <- prepare dbh "SELECT testid FROM hdbctest1 WHERE testname = 'sFetchAllRows' ORDER BY testid"
       execute sth []
       results <- fetchAllRows sth
       assertEqual "" rows results
                               )
    where rows = map (\x -> [iToSql x]) [1..9]

testFetchAllRows' = dbTestCase (\dbh ->
    do sth <- prepare dbh "INSERT INTO hdbctest1 VALUES ('sFetchAllRows2', ?, NULL, NULL)"
       executeMany sth rows
       commit dbh
       sth <- prepare dbh "SELECT testid FROM hdbctest1 WHERE testname = 'sFetchAllRows2' ORDER BY testid"
       execute sth []
       results <- fetchAllRows' sth
       assertEqual "" rows results
                               )
    where rows = map (\x -> [iToSql x]) [1..9]

basicTransactions = dbTestCase (\dbh ->
    do assertBool "Connected database does not support transactions; skipping transaction test" (dbTransactionSupport dbh)
       sth <- prepare dbh "INSERT INTO hdbctest1 VALUES ('basicTransactions', ?, NULL, NULL)"
       execute sth [iToSql 0]
       commit dbh
       qrysth <- prepare dbh "SELECT testid FROM hdbctest1 WHERE testname = 'basicTransactions' ORDER BY testid"
       execute qrysth []
       fetchAllRows qrysth >>= (assertEqual "initial commit" [[toSql "0"]])

       -- Now try a rollback
       executeMany sth rows
       rollback dbh
       execute qrysth []
       fetchAllRows qrysth >>= (assertEqual "rollback" [[toSql "0"]])

       -- Now try another commit
       executeMany sth rows
       commit dbh
       execute qrysth []
       fetchAllRows qrysth >>= (assertEqual "final commit" ([SqlString "0"]:rows))
                               )
    where rows = map (\x -> [iToSql $ x]) [1..9]

testWithTransaction = dbTestCase (\dbh ->
    do assertBool "Connected database does not support transactions; skipping transaction test" (dbTransactionSupport dbh)
       sth <- prepare dbh "INSERT INTO hdbctest1 VALUES ('withTransaction', ?, NULL, NULL)"
       execute sth [toSql "0"]
       commit dbh
       qrysth <- prepare dbh "SELECT testid FROM hdbctest1 WHERE testname = 'withTransaction' ORDER BY testid"
       execute qrysth []
       fetchAllRows qrysth >>= (assertEqual "initial commit" [[toSql "0"]])
       
       -- Let's try a rollback.
       catch (withTransaction dbh (\_ -> do executeMany sth rows
                                            fail "Foo"))
             (\SomeException{} -> return ())
       execute qrysth []
       fetchAllRows qrysth >>= (assertEqual "rollback" [[SqlString "0"]])

       -- And now a commit.
       withTransaction dbh (\_ -> executeMany sth rows)
       execute qrysth []
       fetchAllRows qrysth >>= (assertEqual "final commit" ([iToSql 0]:rows))
                               )
    where rows = map (\x -> [iToSql x]) [1..9]
       
tests = TestList
        [
         TestLabel "openClosedb" openClosedb,
         TestLabel "multiFinish" multiFinish,
         TestLabel "basicQueries" basicQueries,
         TestLabel "createTable" createTable,
         TestLabel "runReplace" runReplace,
         TestLabel "executeReplace" executeReplace,
         TestLabel "executeMany" testExecuteMany,
         TestLabel "fetchAllRows" testFetchAllRows,
         TestLabel "fetchAllRows'" testFetchAllRows',
         TestLabel "basicTransactions" basicTransactions,
         TestLabel "withTransaction" testWithTransaction,
         TestLabel "dropTable" dropTable
         ]
