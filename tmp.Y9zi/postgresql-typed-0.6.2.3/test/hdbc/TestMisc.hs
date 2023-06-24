module TestMisc(tests, setup) where
import Test.HUnit
import Database.HDBC
import TestUtils
import System.IO
import Control.Exception
import Data.Char
import Control.Monad
import qualified Data.Map as Map

rowdata = 
    [[SqlInt32 0, toSql "Testing", SqlNull],
     [SqlInt32 1, toSql "Foo", SqlInt32 5],
     [SqlInt32 2, toSql "Bar", SqlInt32 9]]

colnames = ["testid", "teststring", "testint"]
alrows :: [[(String, SqlValue)]]
alrows = map (zip colnames) rowdata

setup f = dbTestCase $ \dbh ->
   do run dbh "CREATE TABLE hdbctest2 (testid INTEGER PRIMARY KEY NOT NULL, teststring TEXT, testint INTEGER)" []
      sth <- prepare dbh "INSERT INTO hdbctest2 VALUES (?, ?, ?)"
      executeMany sth rowdata
      finish sth
      commit dbh
      finally (f dbh)
              (do run dbh "DROP TABLE hdbctest2" []
                  commit dbh
              )

cloneTest dbh a =
    do dbh2 <- clone dbh
       finally (handleSqlError (a dbh2))
               (handleSqlError (disconnect dbh2))

testgetColumnNames = setup $ \dbh ->
   do sth <- prepare dbh "SELECT * from hdbctest2"
      execute sth []
      cols <- getColumnNames sth
      finish sth
      ["testid", "teststring", "testint"] @=? map (map toLower) cols

testdescribeResult = setup $ \dbh -> when (not ((hdbcDriverName dbh) `elem`
                                               ["sqlite3"])) $
   do sth <- prepare dbh "SELECT * from hdbctest2"
      execute sth []
      cols <- describeResult sth
      ["testid", "teststring", "testint"] @=? map (map toLower . fst) cols
      let coldata = map snd cols
      assertBool "r0 type" (colType (coldata !! 0) `elem`
                            [SqlBigIntT, SqlIntegerT])
      assertBool "r1 type" (colType (coldata !! 1) `elem`
                            [SqlVarCharT, SqlLongVarCharT])
      assertBool "r2 type" (colType (coldata !! 2) `elem`
                            [SqlBigIntT, SqlIntegerT])
      finish sth

testdescribeTable = setup $ \dbh -> when (not ((hdbcDriverName dbh) `elem`
                                               ["sqlite3"])) $
   do cols <- describeTable dbh "hdbctest2"
      ["testid", "teststring", "testint"] @=? map (map toLower . fst) cols
      let coldata = map snd cols
      assertBool "r0 type" (colType (coldata !! 0) `elem`
                            [SqlBigIntT, SqlIntegerT])
      assertEqual "r0 nullable" (Just False) (colNullable (coldata !! 0))
      assertBool "r1 type" (colType (coldata !! 1) `elem`
                            [SqlVarCharT, SqlLongVarCharT])
      assertEqual "r1 nullable" (Just True) (colNullable (coldata !! 1))
      assertBool "r2 type" (colType (coldata !! 2) `elem`
                           [SqlBigIntT, SqlIntegerT])
      assertEqual "r2 nullable" (Just True) (colNullable (coldata !! 2))

testquickQuery = setup $ \dbh ->
    do results <- quickQuery dbh "SELECT * from hdbctest2 ORDER BY testid" []
       rowdata @=? results

testfetchRowAL = setup $ \dbh ->
    do sth <- prepare dbh "SELECT * from hdbctest2 ORDER BY testid" 
       execute sth []
       fetchRowAL sth >>= (Just (head alrows) @=?)
       fetchRowAL sth >>= (Just (alrows !! 1) @=?)
       fetchRowAL sth >>= (Just (alrows !! 2) @=?)
       fetchRowAL sth >>= (Nothing @=?)
       finish sth

testfetchRowMap = setup $ \dbh ->
    do sth <- prepare dbh "SELECT * from hdbctest2 ORDER BY testid" 
       execute sth []
       fetchRowMap sth >>= (Just (Map.fromList $ head alrows) @=?)
       fetchRowMap sth >>= (Just (Map.fromList $ alrows !! 1) @=?)
       fetchRowMap sth >>= (Just (Map.fromList $ alrows !! 2) @=?)
       fetchRowMap sth >>= (Nothing @=?)
       finish sth

testfetchAllRowsAL = setup $ \dbh ->
    do sth <- prepare dbh "SELECT * from hdbctest2 ORDER BY testid"
       execute sth []
       fetchAllRowsAL sth >>= (alrows @=?)

testfetchAllRowsMap = setup $ \dbh ->
    do sth <- prepare dbh "SELECT * from hdbctest2 ORDER BY testid"
       execute sth []
       fetchAllRowsMap sth >>= (map (Map.fromList) alrows @=?)

testexception = setup $ \dbh ->
    catchSql (do sth <- prepare dbh "SELECT invalidcol FROM hdbctest2"
                 execute sth []
                 assertFailure "No exception was raised"
             )
             (\e -> commit dbh)

testrowcount = setup $ \dbh ->
    do r <- run dbh "UPDATE hdbctest2 SET testint = 25 WHERE testid = 20" []
       assertEqual "UPDATE with no change" 0 r
       r <- run dbh "UPDATE hdbctest2 SET testint = 26 WHERE testid = 0" []
       assertEqual "UPDATE with 1 change" 1 r
       r <- run dbh "UPDATE hdbctest2 SET testint = 27 WHERE testid <> 0" []
       assertEqual "UPDATE with 2 changes" 2 r
       commit dbh
       res <- quickQuery dbh "SELECT * from hdbctest2 ORDER BY testid" []
       assertEqual "final results"
         [[SqlInt32 0, toSql "Testing", SqlInt32 26],
          [SqlInt32 1, toSql "Foo", SqlInt32 27],
          [SqlInt32 2, toSql "Bar", SqlInt32 27]] res
                       
{- Since we might be running against a live DB, we can't look at a specific
list here (though a SpecificDB test case may be able to).  We can ensure
that our test table is, or is not, present, as appropriate. -}
                                      
testgetTables1 = setup $ \dbh ->
    do r <- getTables dbh
       True @=? "hdbctest2" `elem` r

testgetTables2 = dbTestCase $ \dbh ->
    do r <- getTables dbh
       False @=? "hdbctest2" `elem` r

testclone = setup $ \dbho -> cloneTest dbho $ \dbh ->
    do results <- quickQuery dbh "SELECT * from hdbctest2 ORDER BY testid" []
       rowdata @=? results

testnulls = setup $ \dbh ->
    do let dn = hdbcDriverName dbh
       when (not (dn `elem` ["postgresql", "odbc", "postgresql-typed"])) (
          do sth <- prepare dbh "INSERT INTO hdbctest2 VALUES (?, ?, ?)"
             executeMany sth rows
             finish sth
             res <- quickQuery dbh "SELECT * from hdbctest2 WHERE testid > 99 ORDER BY testid" []
             seq (length res) rows @=? res
                                             )
    where rows = [[SqlInt32 100, SqlString "foo\NULbar", SqlNull],
                  [SqlInt32 101, SqlString "bar\NUL", SqlNull],
                  [SqlInt32 102, SqlString "\NUL", SqlNull],
                  [SqlInt32 103, SqlString "\xFF", SqlNull],
                  [SqlInt32 104, SqlString "regular", SqlNull]]
       
testunicode = setup $ \dbh ->
      do sth <- prepare dbh "INSERT INTO hdbctest2 VALUES (?, ?, ?)"
         executeMany sth rows
         finish sth
         res <- quickQuery dbh "SELECT * from hdbctest2 WHERE testid > 99 ORDER BY testid" []
         seq (length res) rows @=? res
    where rows = [[SqlInt32 100, SqlString "foo\x263a", SqlNull],
                  [SqlInt32 101, SqlString "bar\x00A3", SqlNull],
                  [SqlInt32 102, SqlString (take 263 (repeat 'a')), SqlNull]]

tests = TestList [TestLabel "getColumnNames" testgetColumnNames,
                  TestLabel "describeResult" testdescribeResult,
                  TestLabel "describeTable" testdescribeTable,
                  TestLabel "quickQuery" testquickQuery,
                  TestLabel "fetchRowAL" testfetchRowAL,
                  TestLabel "fetchRowMap" testfetchRowMap,
                  TestLabel "fetchAllRowsAL" testfetchAllRowsAL,
                  TestLabel "fetchAllRowsMap" testfetchAllRowsMap,
                  TestLabel "sql exception" testexception,
                  TestLabel "clone" testclone,
                  TestLabel "update rowcount" testrowcount,
                  TestLabel "get tables1" testgetTables1,
                  TestLabel "get tables2" testgetTables2,
                  TestLabel "nulls" testnulls,
                  TestLabel "unicode" testunicode]
