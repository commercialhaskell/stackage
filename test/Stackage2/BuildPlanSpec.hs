{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Stackage2.BuildPlanSpec (spec) where

import Stackage2.BuildPlan
import Stackage2.Prelude
import Stackage2.PackageConstraints
import Stackage2.UpdateBuildPlan
import Test.Hspec
import qualified Data.Yaml as Y
import Control.Exception (evaluate)
import Distribution.Version (anyVersion)
import qualified Data.Map as Map

spec :: Spec
spec = it "works" $ do
    pc <- defaultPackageConstraints
    bp <- newBuildPlan pc
    let bs = Y.encode bp
        mbp' = Y.decode bs

    bp' <- maybe (error "decoding failed") return mbp'

    let allPackages = Map.keysSet (bpExtra bp) ++ Map.keysSet (bpExtra bp')
    forM_ allPackages $ \name ->
        (name, lookup name (bpExtra bp')) `shouldBe`
        (name, lookup name (bpExtra $ () <$ bp))

    mbp' `shouldBe` Just (() <$ bp)
    bp2 <- newBuildPlan $ updatePackageConstraints bp
    dropVersionRanges bp2 `shouldBe` dropVersionRanges bp
  where
    dropVersionRanges bp =
        bp { bpExtra = map go $ bpExtra bp }
      where
        go pb = pb { pbVersionRange = anyVersion }
