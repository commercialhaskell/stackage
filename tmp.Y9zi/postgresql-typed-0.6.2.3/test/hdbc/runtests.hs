{- arch-tag: Test runner
-}

module Main where 

import Test.HUnit
import System.Exit
import Tests
import TestUtils

main = do
  printDBInfo
  r <- runTestTT tests
  if errors r == 0 && failures r == 0
    then exitSuccess
    else exitFailure
