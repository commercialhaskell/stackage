{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The constraints on package selection for a new build plan.
module Stackage2.PackageConstraints
    ( PackageConstraints (..)
    , defaultPackageConstraints
    ) where

import           Stackage2.Prelude
import qualified Stackage.Config as Old
import qualified Stackage.Types  as Old

data PackageConstraints = PackageConstraints
    { pcPackages :: Map PackageName (VersionRange, Maintainer)
    -- ^ This does not include core packages or dependencies, just packages
    -- added by some maintainer.
    , pcExpectedFailures :: Set PackageName
    -- ^ At some point in the future, we should split this into Haddock
    -- failures, test failures, etc.
    }

-- | The proposed plan from the requirements provided by contributors.
defaultPackageConstraints :: PackageConstraints
defaultPackageConstraints = PackageConstraints
    { pcPackages = fmap (Maintainer . pack . Old.unMaintainer)
               <$> Old.defaultStablePackages ghcVer False
    , pcExpectedFailures = Old.defaultExpectedFailures ghcVer False
    }
  where
    ghcVer = Old.GhcMajorVersion 7 8
