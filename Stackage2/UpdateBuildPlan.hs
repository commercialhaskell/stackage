{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Take an existing build plan and bump all packages to the newest version in
-- the same major version number.
module Stackage2.UpdateBuildPlan
    ( updateBuildConstraints
    , updateBuildPlan
    ) where

import Stackage2.Prelude
import Stackage2.BuildPlan
import Stackage2.BuildConstraints
import Stackage2.PackageDescription
import Distribution.Version (orLaterVersion, earlierVersion, anyVersion)
import qualified Data.Map as Map

updateBuildPlan :: BuildPlan -> IO BuildPlan
updateBuildPlan = newBuildPlan . updateBuildConstraints

updateBuildConstraints :: BuildPlan -> BuildConstraints
updateBuildConstraints BuildPlan {..} =
    BuildConstraints {..}
  where
    bcSystemInfo = bpSystemInfo
    bcPackages = Map.keysSet bpPackages

    bcPackageConstraints name = PackageConstraints
        { pcVersionRange = addBumpRange (maybe anyVersion pcVersionRange moldPC)
        , pcMaintainer = moldPC >>= pcMaintainer
        , pcTests = maybe ExpectSuccess pcTests moldPC
        , pcHaddocks = maybe ExpectSuccess pcHaddocks moldPC
        , pcBuildBenchmarks = maybe True pcBuildBenchmarks moldPC
        , pcFlagOverrides = maybe mempty pcFlagOverrides moldPC
        }
      where
        moldBP = lookup name bpPackages
        moldPC = ppConstraints <$> moldBP

        addBumpRange oldRange =
            case moldBP of
                Nothing -> oldRange
                Just bp -> intersectVersionRanges oldRange
                         $ bumpRange $ ppVersion bp

    bumpRange version = intersectVersionRanges
        (orLaterVersion version)
        (earlierVersion $ bumpVersion version)
    bumpVersion (Version (x:y:_) _) = Version [x, y + 1] []
    bumpVersion (Version [x] _) = Version [x, 1] []
    bumpVersion (Version [] _) = assert False $ Version [1, 0] []
