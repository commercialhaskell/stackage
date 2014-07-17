module Stackage.Types
    ( module X
    , module Stackage.Types
    ) where

import           Data.Map                        as X (Map)
import           Data.Map                        (unionWith)
import           Data.Monoid                     (Monoid (..))
import           Data.Set                        as X (Set)
import           Data.Version                    as X (Version)
import           Distribution.Package            as X (PackageIdentifier (..),
                                                       PackageName (..))
import           Distribution.PackageDescription (GenericPackageDescription)
import           Distribution.Version            as X (VersionRange (..))
import           Distribution.Version            (intersectVersionRanges)

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
    , piBuildToolsExe :: Set Executable -- ^ required just for building executable/lib
    , piBuildToolsAll :: Set Executable -- ^ required for all stanzas
    , piGPD        :: Maybe GenericPackageDescription
    , piExecs      :: Set Executable
    , piGithubUser :: [String]
    }
    deriving (Show, Eq)

newtype Executable = Executable String
    deriving (Show, Eq, Ord)

-- | Information on a package we're going to build.
data BuildInfo = BuildInfo
    { biVersion    :: Version
    , biUsers      :: [PackageName]
    , biMaintainer :: Maintainer
    , biDeps       :: Map PackageName VersionRange
    , biGithubUser :: [String]
    , biHasTests   :: Bool
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
    { iiCore         :: Map PackageName (Maybe Version)
    , iiPackages     :: Map PackageName SelectedPackageInfo
    , iiOptionalCore :: Map PackageName Version
      -- ^ This is intended to hold onto packages which might be automatically
      -- provided in the global package database. In practice, this would be
      -- Haskell Platform packages provided by distributions.
    , iiPackageDB    :: PackageDB
    }

data SelectedPackageInfo = SelectedPackageInfo
    { spiVersion    :: Version
    , spiMaintainer :: Maintainer
    , spiGithubUser :: [String]
    , spiHasTests   :: Bool
    }
    deriving (Show, Read)

data BuildPlan = BuildPlan
    { bpTools        :: [String]
    , bpPackages     :: Map PackageName SelectedPackageInfo
    , bpCore         :: Map PackageName (Maybe Version)
    , bpOptionalCore :: Map PackageName Version
      -- ^ See 'iiOptionalCore'
    , bpSkippedTests :: Set PackageName
    }

-- | Email address of a Stackage maintainer.
newtype Maintainer = Maintainer { unMaintainer :: String }
    deriving (Show, Eq, Ord, Read)

data SelectSettings = SelectSettings
    { haskellPlatformDir     :: FilePath
    , flags                  :: Map PackageName Version -> Set String
    -- ^ Compile flags which should be turned on. Takes a Map providing the
    -- core packages so that flags can be set appropriately.
    , disabledFlags          :: Set String
    -- ^ Compile flags which should always be disabled.
    , extraCore              :: Set PackageName
    , ignoreUpgradeableCore  :: Bool
    -- ^ Do not pin down the versions of upgradeable core packages.
    , requireHaskellPlatform :: Bool
    , allowedPackage         :: GenericPackageDescription -> Either String ()
    -- ^ Checks if a package is allowed into the distribution. By default, we
    -- allow all packages in, though this could be used to filter out certain
    -- untrusted packages, or packages with an unacceptable license.
    --
    -- Returns a reason for stripping in Left, or Right if the package is
    -- allowed.
    , expectedFailuresSelect :: Set PackageName
    , excludedPackages       :: Set PackageName
    -- ^ Packages which should be dropped from the list of stable packages,
    -- even if present via the Haskell Platform or @stablePackages@. If these
    -- packages are dependencies of others, they will still be included.
    , stablePackages         :: Bool -- ^ require Haskell Platform?
                             -> Map PackageName (VersionRange, Maintainer)
    , useGlobalDatabase      :: Bool
    -- ^ Instead of checking the Haskell Platform file for core packages, query
    -- the global database. For this to be reliable, you should only have
    -- default packages in your global database. Default is @False@.
    , skippedTests           :: Set PackageName
    -- ^ Do not build or run test suites, usually in order to avoid a
    -- dependency.
    , selectGhcVersion       :: GhcMajorVersion
    , selectTarballDir       :: FilePath
    -- ^ Directory containing replacement tarballs.
    }

data BuildStage = BSTools | BSBuild | BSTest

data BuildSettings = BuildSettings
    { sandboxRoot           :: FilePath
    , extraArgs             :: BuildStage -> [String]
    , expectedFailuresBuild :: Set PackageName
    , testWorkerThreads     :: Int
    -- ^ How many threads to spawn for running test suites.
    , buildDocs             :: Bool
    -- ^ Build docs as part of the test procedure.
    , tarballDir            :: FilePath
    -- ^ Directory containing replacement tarballs.
    , cabalFileDir          :: Maybe FilePath
    -- ^ Directory to place cabal files in
    }

-- | A wrapper around a @Map@ providing a better @Monoid@ instance.
newtype PackageMap = PackageMap { unPackageMap :: Map PackageName (VersionRange, Maintainer) }

instance Monoid PackageMap where
    mempty = PackageMap mempty
    PackageMap x `mappend` PackageMap y =
        PackageMap $ unionWith go x y
      where
        go (r1, m1) (r2, _) = (intersectVersionRanges r1 r2, m1)

-- | GHC major version. For example, for GHC 7.4.2, this would be 7 4.
data GhcMajorVersion = GhcMajorVersion Int Int
    deriving (Show, Ord, Eq)
