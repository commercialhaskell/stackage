{- arch-tag: Tests main file
-}

module Tests(tests) where
import Test.HUnit
import qualified Testbasics
import qualified TestSbasics
import qualified TestMisc
import qualified TestTime

test1 = TestCase ("x" @=? "x")

tests = TestList
  [ TestLabel "test1" test1
  , TestLabel "String basics" TestSbasics.tests
  , TestLabel "SqlValue basics" Testbasics.tests
  , TestLabel "Misc tests" TestMisc.tests
  , TestLabel "Time tests" TestTime.tests
  ]
