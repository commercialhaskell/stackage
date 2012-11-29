module Stackage.Util where

import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as TarEntry
import           Control.Monad           (guard, when)
import           Data.List               (stripPrefix)
import qualified Data.Map                as Map
import qualified Data.Set                as Set
import           Data.Version            (showVersion)
import           Distribution.Text       (simpleParse)
import           Distribution.Version    (thisVersion)
import           Stackage.Types
import           System.Directory        (doesDirectoryExist,
                                          removeDirectoryRecursive)
import           System.Directory        (getAppUserDataDirectory)
import           System.FilePath         ((</>))

identsToRanges :: Set PackageIdentifier -> Map PackageName (VersionRange, Maintainer)
identsToRanges =
    Map.unions . map go . Set.toList
  where
    go (PackageIdentifier package version) = Map.singleton package (thisVersion version, Maintainer "Haskell Platform")

packageVersionString :: (PackageName, Version) -> String
packageVersionString (PackageName p, v) = concat [p, "-", showVersion v]

rm_r :: FilePath -> IO ()
rm_r fp = do
    exists <- doesDirectoryExist fp
    when exists $ removeDirectoryRecursive fp

getCabalRoot :: IO FilePath
getCabalRoot = getAppUserDataDirectory "cabal"

-- | Name of the 00-index.tar downloaded from Hackage.
getTarballName :: IO FilePath
getTarballName = do
    c <- getCabalRoot
    return $ c </> "packages" </> "hackage.haskell.org" </> "00-index.tar"

stableRepoName, extraRepoName :: String
stableRepoName = "stackage"
extraRepoName = "stackage-extra"

-- | Locations for the stackage and stackage-extra tarballs
getStackageTarballNames :: IO (FilePath, FilePath)
getStackageTarballNames = do
    c <- getCabalRoot
    let f x = c </> "packages" </> x </> "00-index.tar"
    return (f stableRepoName, f extraRepoName)

getPackageVersion :: Tar.Entry -> Maybe (PackageName, Version)
getPackageVersion e = do
    let (package', s1) = break (== '/') fp
        package = PackageName package'
    s2 <- stripPrefix "/" s1
    let (version', s3) = break (== '/') s2
    version <- simpleParse version'
    s4 <- stripPrefix "/" s3
    guard $ s4 == package' ++ ".cabal"
    Just (package, version)
  where
    fp = TarEntry.fromTarPathToPosixPath $ TarEntry.entryTarPath e

-- | If a package cannot be parsed or is not found, the default value for
-- whether it has a test suite. We default to @True@ since, worst case
-- scenario, this just means a little extra time trying to run a suite that's
-- not there. Defaulting to @False@ would result in silent failures.
defaultHasTestSuites :: Bool
defaultHasTestSuites = True

packageDir = (</> "package-db") . sandboxRoot
libDir = (</> "lib") . sandboxRoot
binDir = (</> "bin") . sandboxRoot
dataDir = (</> "share") . sandboxRoot
docDir x = sandboxRoot x </> "share" </> "doc" </> "$pkgid"

addCabalArgs settings rest
    = "--package-db=clear"
    : "--package-db=global"
    : ("--package-db=" ++ packageDir settings)
    : ("--libdir=" ++ libDir settings)
    : ("--bindir=" ++ binDir settings)
    : ("--datadir=" ++ dataDir settings)
    : ("--docdir=" ++ docDir settings)
    : extraArgs settings ++ rest
