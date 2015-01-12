-- | Build everything with Shake.

module Stackage.ShakeBuild where

import           Stackage.BuildConstraints
import           Stackage.BuildPlan
import           Stackage.PackageDescription
import           Stackage.PerformBuild (PerformBuild(..),copyBuiltInHaddocks)

import           Control.Monad hiding (forM_)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Development.Shake hiding (doesDirectoryExist)
import           Distribution.Package (PackageName)
import           Distribution.Text (display)
import qualified Filesystem.Path.CurrentOS as FP
import           Stackage.Prelude (unFlagName)
import           System.Directory
import           System.Environment

-- | Run the shake builder.
performBuild :: PerformBuild -> IO ()
performBuild pb = do
    shakeDir <- fmap (<//> "shake/") (getCurrentDirectory >>= canonicalizePath)
    createDirectoryIfMissing True shakeDir
    withArgs
        []
        (shakeArgs
             shakeOptions {shakeFiles = shakeDir}
             (shakePlan pb shakeDir))

-- | The complete build plan as far as Shake is concerned.
shakePlan :: PerformBuild -> FilePath -> Rules ()
shakePlan pb shakeDir = do
    fetched <- target (targetForFetched shakeDir) $
               fetchedTarget shakeDir pb
    db <- target
              (targetForDb shakeDir pb)
              (databaseTarget shakeDir pb)
    _ <- forM corePackages $
         \name ->
              let fp =
                      targetForPackage shakeDir name
              in target fp (makeFile fp)
    packageTargets <- forM normalPackages $
                      \(name,plan) ->
                           target
                               (targetForPackage shakeDir name)
                               (do need [db, fetched]
                                   packageTarget pb shakeDir name plan)
    want packageTargets
    where corePackages =
              M.keys $ siCorePackages $ bpSystemInfo $ pbPlan pb
          normalPackages =
              filter (not . (`elem` corePackages) . fst) $
              M.toList $ bpPackages $ pbPlan pb

-- | Initialize the database if there one needs to be, and in any case
-- create the target file.
databaseTarget :: FilePath -> PerformBuild -> Action ()
databaseTarget shakeDir pb =
    if pbGlobalInstall pb
       then liftIO (createDirectoryIfMissing True dir)
       else do liftIO (createDirectoryIfMissing True (dir))
               liftIO (removeDirectoryRecursive dir)
               () <- cmd "ghc-pkg" "init" dir
               liftIO (copyBuiltInHaddocks (FP.decodeString (pbDocDir pb)))
  where dir = targetForDb shakeDir pb

-- | Make sure all package archives have been fetched.
fetchedTarget :: FilePath -> PerformBuild -> Action ()
fetchedTarget shakeDir pb = do
    () <- cmd "cabal" "fetch" "--no-dependencies" $
          map
              (\(name,plan) ->
                    display name ++
                    "-" ++
                    display (ppVersion plan))
              (M.toList
                   (bpPackages
                        (pbPlan pb)))
    makeFile (targetForFetched shakeDir)

-- | Build, test and generate documentation for the package.
packageTarget :: PerformBuild -> FilePath -> PackageName -> PackagePlan -> Action ()
packageTarget pb shakeDir name plan = do
    need (map (targetForPackage shakeDir)
              (M.keys (sdPackages (ppDesc plan))))
    pwd <- liftIO getCurrentDirectory
    env <- liftIO (fmap (Env . (++ defaultEnv pwd)) getEnvironment)
    () <- cmd (Cwd shakeDir) "cabal" "unpack" nameVer
    () <- cmd cwd env "cabal" "configure" (opts pwd)
    () <- cmd cwd env "cabal" "build"
    () <- cmd cwd env "cabal" "copy"
    () <- cmd cwd env "cabal" "register"
    makeFile (targetForPackage shakeDir name)
    where cwd = Cwd pkgDir
          defaultEnv pwd = [("HASKELL_PACKAGE_SANDBOX",pwd <//> targetForDb shakeDir pb)]
          opts pwd = ["--package-db=clear"
                     ,"--package-db=global"
                     ,"--libdir=" ++ pwd <//> pbLibDir pb
                     ,"--bindir=" ++ pwd <//> pbBinDir pb
                     ,"--datadir=" ++ pwd <//> pbDataDir pb
                     ,"--docdir=" ++ pwd <//> pbDocDir pb
                     ,"--flags=" ++ flags] ++
                     ["--package-db=" ++ pwd <//> targetForDb shakeDir pb
                     |not (pbGlobalInstall pb)]
          pkgDir =
              shakeDir <//> nameVer
          nameVer =
              display name ++
              "-" ++
              display (ppVersion plan)
          flags = unwords $ map go $ M.toList (pcFlagOverrides (ppConstraints plan))
                where
                  go (name', isOn) = concat
                      [ if isOn then "" else "-"
                      , T.unpack (unFlagName name')
                      ]

-- | Get the target file for confirming that all packages have been
-- pre-fetched.
targetForFetched :: FilePath -> FilePath
targetForFetched shakeDir =
    shakeDir <//> "fetched"

-- | Get the target file for a package.
targetForPackage :: FilePath -> PackageName -> FilePath
targetForPackage shakeDir name =
    shakeDir <//> "packages" <//> display name

-- | Get a package database path.
targetForDb :: FilePath -> PerformBuild -> FilePath
targetForDb shakeDir pb =
    if pbGlobalInstall pb
       then shakeDir <//> "pkgdb-global"
       else FP.encodeString (pbInstallDest pb) <//> "pkgdb"

-- | Declare a target, returning the target name.
target :: FilePattern -> Action () -> Rules FilePattern
target name act = do
    name *> const act
    return name

-- | Make a file of this name.
makeFile :: FilePath -> Action ()
makeFile fp = liftIO $ writeFile fp ""

pbBinDir, pbLibDir, pbDataDir, pbDocDir :: PerformBuild -> FilePath
pbBinDir pb = FP.encodeString (pbInstallDest pb) <//> "bin"
pbLibDir pb = FP.encodeString (pbInstallDest pb) <//> "lib"
pbDataDir pb = FP.encodeString (pbInstallDest pb) <//> "share"
pbDocDir pb = FP.encodeString (pbInstallDest pb) <//> "doc"
