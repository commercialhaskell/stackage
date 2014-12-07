{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Stackage2.BuildPlanSpec (spec) where

import Stackage2.BuildPlan
import Stackage2.Prelude
import Stackage2.PackageConstraints
import Test.Hspec
import qualified Data.Yaml as Y
import Control.Exception (evaluate)

spec :: Spec
spec = it "works" $ do
    bp <- defaultPackageConstraints >>= newBuildPlan
    let bs = Y.encode bp
        mbp' = Y.decode bs
    mbp' `shouldBe` Just (() <$ bp)
