{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Stackage2.BuildPlanSpec (spec) where

import Stackage2.BuildPlan
import Stackage2.Prelude
import Test.Hspec
import qualified Data.Yaml as Y
import Control.Exception (evaluate)

spec :: Spec
spec = it "works" $ do
    bp <- newBuildPlan
    let bs = Y.encode bp
        mbp' = Y.decode bs
    Y.encodeFile "myplan.yaml" bp
    mbp' `shouldBe` Just (() <$ bp)
