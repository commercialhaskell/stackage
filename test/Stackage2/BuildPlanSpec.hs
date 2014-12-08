{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Stackage2.BuildPlanSpec (spec) where

import Stackage2.BuildPlan
import Stackage2.Prelude
import Stackage2.BuildConstraints
import Stackage2.UpdateBuildPlan
import Test.Hspec
import qualified Data.Yaml as Y
import Control.Exception (evaluate)
import Distribution.Version (anyVersion)
import qualified Data.Map as Map

spec :: Spec
spec = it "works" $ do
    bc <- defaultBuildConstraints
    bp <- newBuildPlan bc
    let bs = Y.encode bp
        ebp' = Y.decodeEither bs

    bp' <- either error return ebp'

    let allPackages = Map.keysSet (bpPackages bp) ++ Map.keysSet (bpPackages bp')
    forM_ allPackages $ \name ->
        (name, lookup name (bpPackages bp')) `shouldBe`
        (name, lookup name (bpPackages (() <$ bp)))

    bp' `shouldBe` (() <$ bp)
    bp2 <- updateBuildPlan bp
    dropVersionRanges bp2 `shouldBe` dropVersionRanges bp
  where
    dropVersionRanges bp =
        bp { bpPackages = map go $ bpPackages bp }
      where
        go pb = pb { pbPackageConstraints = go' $ pbPackageConstraints pb }
        go' pc = pc { pcVersionRange = anyVersion }
