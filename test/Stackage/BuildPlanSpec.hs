{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Stackage.BuildPlanSpec (spec) where

import Stackage.BuildPlan
import Stackage.Prelude
import Stackage.BuildConstraints
import Stackage.UpdateBuildPlan
import Test.Hspec
import qualified Data.Yaml as Y
import Distribution.Version (anyVersion)
import qualified Data.Map as Map

spec :: Spec
spec = it "works" $ do
    bc <- defaultBuildConstraints (error "manager should not be used")
    bp <- newBuildPlan bc
    let bs = Y.encode bp
        ebp' = Y.decodeEither bs

    bp' <- either error return ebp'

    let allPackages = Map.keysSet (bpPackages bp) ++ Map.keysSet (bpPackages bp')
    forM_ allPackages $ \name ->
        (name, lookup name (bpPackages bp')) `shouldBe`
        (name, lookup name (bpPackages bp))

    bpGithubUsers bp' `shouldBe` bpGithubUsers bp
    when (bp' /= bp) $ error "bp' /= bp"
    bp2 <- updateBuildPlan bp
    when (dropVersionRanges bp2 /= dropVersionRanges bp) $ error "bp2 /= bp"
  where
    dropVersionRanges bp =
        bp { bpPackages = map go $ bpPackages bp }
      where
        go pb = pb { ppConstraints = go' $ ppConstraints pb }
        go' pc = pc { pcVersionRange = anyVersion }
