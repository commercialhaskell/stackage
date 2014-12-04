{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Stackage2.PackageIndexSpec (spec) where

import Stackage2.PackageIndex
import Stackage2.Prelude
import Test.Hspec

spec :: Spec
spec = it "works" $ (runResourceT $ sourcePackageIndex $$ sinkNull :: IO ())
