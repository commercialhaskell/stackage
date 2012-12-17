module Stackage.Types
    ( module X
    , module Stackage.Types
    ) where

import           Data.Map             as X (Map)
import           Data.Map             (unionWith)
import           Data.Monoid          (Monoid (..))
import           Data.Set             as X (Set)
import           Data.Version         as X (Version)
import           Distribution.Package as X (PackageIdentifier (..),
                                            PackageName (..))
import           Distribution.Version as X (VersionRange (..))

newtype PackageDB = PackageDB (Map PackageName PackageInfo)
    deriving (Show, Eq)

instance Monoid PackageDB where
    mempty = PackageDB mempty
    PackageDB x `mappend` PackageDB y =
        PackageDB $ unionWith newest x y
      where
        newest pi1 pi2
            | piVersion pi1 > piVersion pi2 = pi1
            | otherwise = pi2

data PackageInfo = PackageInfo
    { piVersion    :: Version
    , piDeps       :: Map PackageName VersionRange
    , piHasTests   :: Bool
    , piBuildTools :: Set PackageName
    }
    deriving (Show, Eq)

-- | Information on a package we're going to build.
data BuildInfo = BuildInfo
    { biVersion    :: Version
    , biUsers      :: [PackageName]
    , biMaintainer :: Maintainer
    , biDeps       :: Map PackageName VersionRange
    }

data HaskellPlatform = HaskellPlatform
    { hpcore :: Set PackageIdentifier
    , hplibs :: Set PackageIdentifier
    }
    deriving (Show, Eq, Ord)
instance Monoid HaskellPlatform where
    mempty = HaskellPlatform mempty mempty
    HaskellPlatform a x `mappend` HaskellPlatform b y = HaskellPlatform (mappend a b) (mappend x y)

data InstallInfo = InstallInfo
    { iiCore         :: Set PackageName
    , iiPackages     :: Map PackageName (Version, Maintainer)
    , iiOptionalCore :: Map PackageName Version
      -- ^ This is intended to hold onto packages which might be automatically
      -- provided in the global package database. In practice, this would be
      -- Haskell Platform packages provided by distributions.
    , iiPackageDB    :: PackageDB
    }

-- | Email address of a Stackage maintainer.
newtype Maintainer = Maintainer { unMaintainer :: String }
    deriving (Show, Eq, Ord)

data BuildSettings = BuildSettings
    { sandboxRoot            :: FilePath
    , extraBuildArgs         :: [String]
    , extraCore              :: Set PackageName
    , expectedFailures       :: Set PackageName
    , stablePackages         :: Map PackageName (VersionRange, Maintainer)
    , extraArgs              :: [String]
    , haskellPlatformCabal   :: FilePath
    , requireHaskellPlatform :: Bool
    , cleanBeforeBuild       :: Bool
    , excludedPackages       :: Set PackageName
    -- ^ Packages which should be dropped from the list of stable packages,
    -- even if present via the Haskell Platform or @stablePackages@. If these
    -- packages are dependencies of others, they will still be included.
    , testWorkerThreads      :: Int
    -- ^ How many threads to spawn for running test suites.
    , flags                  :: Set String
    -- ^ Compile flags which should be turned on.
    }
