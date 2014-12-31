{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- | Take an existing build plan and bump all packages to the newest version in
-- the same major version number.
module Stackage.UpdateBuildPlan
    ( updateBuildConstraints
    , updateBuildPlan
    ) where

import qualified Data.Map                  as Map
import           Distribution.Version      (anyVersion, earlierVersion,
                                            orLaterVersion)
import           Stackage.BuildConstraints
import           Stackage.BuildPlan
import           Stackage.Prelude

updateBuildPlan :: BuildPlan -> IO BuildPlan
updateBuildPlan = newBuildPlan . updateBuildConstraints

updateBuildConstraints :: BuildPlan -> BuildConstraints
updateBuildConstraints BuildPlan {..} =
    BuildConstraints {..}
  where
    bcSystemInfo = bpSystemInfo
    bcPackages = Map.keysSet bpPackages
    bcGithubUsers = bpGithubUsers

    bcPackageConstraints name = PackageConstraints
        { pcVersionRange = addBumpRange (maybe anyVersion pcVersionRange moldPC)
        , pcMaintainer = moldPC >>= pcMaintainer
        , pcTests = maybe ExpectSuccess pcTests moldPC
        , pcHaddocks = maybe ExpectSuccess pcHaddocks moldPC
        , pcBuildBenchmarks = maybe True pcBuildBenchmarks moldPC
        , pcFlagOverrides = maybe mempty pcFlagOverrides moldPC
        , pcEnableLibProfile = maybe False pcEnableLibProfile moldPC
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
