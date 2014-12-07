{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Take an existing build plan and bump all packages to the newest version in
-- the same major version number.
module Stackage2.UpdateBuildPlan
    ( updatePackageConstraints
    , updateBuildPlan
    ) where

import Stackage2.Prelude
import Stackage2.BuildPlan
import Stackage2.PackageConstraints
import Stackage2.PackageDescription
import Distribution.Version (orLaterVersion, earlierVersion)

updateBuildPlan :: BuildPlan a -> IO (BuildPlan FlatComponent)
updateBuildPlan = newBuildPlan . updatePackageConstraints

updatePackageConstraints :: BuildPlan a -> PackageConstraints
updatePackageConstraints BuildPlan {..} = PackageConstraints
    { pcPackages = flip map bpExtra $ \pb ->
        ( intersectVersionRanges (bumpRange (pbVersion pb)) (pbVersionRange pb)
        , pbMaintainer pb
        )
    , pcCorePackages = bpCore
    , pcCoreExecutables = bpCoreExecutables
    , pcGhcVersion = bpGhcVersion
    , pcOS = bpOS
    , pcArch = bpArch
    , pcTests = maybe ExpectSuccess pbTestState . flip lookup bpExtra
    , pcHaddocks = maybe ExpectSuccess pbHaddockState . flip lookup bpExtra
    , pcBuildBenchmark = maybe True pbTryBuildBenchmark . flip lookup bpExtra
    , pcFlagOverrides = maybe mempty pbFlags . flip lookup bpExtra
    }
  where
    bumpRange version = intersectVersionRanges
        (orLaterVersion version)
        (earlierVersion $ bumpVersion version)
    bumpVersion (Version (x:y:_) _) = Version [x, y + 1] []
    bumpVersion (Version [x] _) = Version [x, 1] []
    bumpVersion (Version [] _) = assert False $ Version [1, 0] []
