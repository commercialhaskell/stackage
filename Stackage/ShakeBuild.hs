{-# LANGUAGE ScopedTypeVariables #-}

-- | Build everything with Shake.

module Stackage.ShakeBuild where

import           Data.Monoid
import           Stackage.BuildConstraints
import           Stackage.BuildPlan
import           Stackage.PackageDescription
import           Stackage.PerformBuild (PerformBuild(..),copyBuiltInHaddocks,renameOrCopy)
import           Stackage.Prelude (unFlagName)

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad hiding (forM_)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Development.Shake hiding (doesFileExist,doesDirectoryExist)
import           Distribution.Package (PackageName)
import           Distribution.Text (display)
import qualified Filesystem.Path.CurrentOS as FP
import           System.Directory
import           System.Environment

-- | Run the shake builder.
performBuild :: PerformBuild -> IO ()
performBuild pb = do
    shakeDir <- fmap (<//> "shake/") (getCurrentDirectory >>= canonicalizePath)
    createDirectoryIfMissing True shakeDir
    haddockFiles <- liftIO (newTVarIO mempty)
    withArgs
        []
        (shakeArgs
             shakeOptions {shakeFiles = shakeDir
                          ,shakeVerbosity = Diagnostic}
             (shakePlan haddockFiles pb shakeDir))

-- | The complete build plan as far as Shake is concerned.
shakePlan :: TVar (Map String FilePath) -> PerformBuild -> FilePath -> Rules ()
shakePlan haddockFiles pb shakeDir = do
    fetched <- target (targetForFetched shakeDir) $
               fetchedTarget shakeDir pb
    db <- target
              (targetForDb' shakeDir)
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
                                   packageTarget haddockFiles pb shakeDir name plan)
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
    do if pbGlobalInstall pb
          then return ()
          else do liftIO (createDirectoryIfMissing True dir)
                  liftIO (removeDirectoryRecursive dir)
                  () <- cmd "ghc-pkg" "init" dir
                  liftIO (copyBuiltInHaddocks (FP.decodeString (pbDocDir pb)))
       makeFile (targetForDb' shakeDir)
  where dir = buildDatabase pb

-- | Build, test and generate documentation for the package.
packageTarget :: TVar (Map String FilePath)
              -> PerformBuild -> FilePath -> PackageName -> PackagePlan
              -> Action ()
packageTarget haddockFiles pb shakeDir name plan = do
    need (map (targetForPackage shakeDir)
              (M.keys (sdPackages (ppDesc plan))))
    pwd <- liftIO getCurrentDirectory
    env <- liftIO (fmap (Env . (++ defaultEnv pwd)) getEnvironment)
    unpack shakeDir nameVer
    configure pkgDir env pb plan
    () <- cmd cwd env "cabal" "build"
    register pkgDir env
    when (pcHaddocks (ppConstraints plan) /= Don'tBuild) $
        (generateHaddocks haddockFiles pb pkgDir env name nameVer)
    makeFile (targetForPackage shakeDir name)
    where cwd =
              Cwd pkgDir
          defaultEnv pwd =
              [ ( "HASKELL_PACKAGE_SANDBOX"
                , pwd <//>
                  buildDatabase pb)
              | pbGlobalInstall pb]
          pkgDir = shakeDir <//> nameVer
          nameVer =
              display name ++
              "-" ++
              display (ppVersion plan)

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

-- | Unpack the package.
unpack :: FilePath -> String -> Action ()
unpack shakeDir nameVer = do
    unpacked <- liftIO (doesDirectoryExist pkgDir)
    unless unpacked (cmd (Cwd shakeDir) "cabal" "unpack" nameVer)
    where pkgDir =
              shakeDir <//> nameVer

-- | Configure the given package.
configure :: FilePath -> CmdOption -> PerformBuild -> PackagePlan -> Action ()
configure pkgDir env pb plan = do
    configured <- liftIO
                      (doesFileExist
                           (pkgDir <//> "dist" <//> "setup-config"))
    unless
        configured
        (do pwd <- liftIO getCurrentDirectory
            cmd (Cwd pkgDir) env "cabal" "configure" (opts pwd))
  where opts pwd =
            [ "--package-db=clear"
            , "--package-db=global"
            , "--libdir=" ++ pwd <//> pbLibDir pb
            , "--bindir=" ++ pwd <//> pbBinDir pb
            , "--datadir=" ++ pwd <//> pbDataDir pb
            , "--docdir=" ++ pwd <//> pbDocDir pb
            , "--flags=" ++ planFlags plan] ++
            ["--package-db=" ++
             pwd <//>
             buildDatabase pb | not (pbGlobalInstall pb)]

-- | Register the package.
--
-- TODO: Do a mutex lock in here. Does Shake already support doing
-- this out of the box?
register :: FilePath -> CmdOption -> Action ()
register pkgDir env =
    do () <- cmd cwd env "cabal" "copy"
       cmd cwd env "cabal" "register"
  where cwd = Cwd pkgDir

-- | Generate haddocks for the package.
generateHaddocks
    :: TVar (Map String FilePath)
    -> PerformBuild
    -> FilePath
    -> CmdOption
    -> PackageName
    -> FilePattern
    -> Action ()
generateHaddocks haddockFiles pb pkgDir env name nameVer = do
    hfs <- liftIO (readTVarIO haddockFiles)
    () <- cmd
              (Cwd pkgDir)
              env
              "cabal"
              "haddock"
              "--hyperlink-source"
              "--html"
              "--hoogle"
              "--html-location=../$pkg-$version/"
              (map
                   (\(pkgVer,hf) ->
                         concat
                             [ "--haddock-options=--read-interface="
                             , "../"
                             , pkgVer
                             , "/,"
                             , hf])
                   (M.toList hfs))
    liftIO
        (renameOrCopy
             (FP.decodeString
                  (pkgDir <//> "dist" <//> "doc" <//> "html" <//> display name))
             (FP.decodeString
                  (pbDocDir pb <//> nameVer)))
    enewPath <- liftIO
                    (try $
                     canonicalizePath
                         (pbDocDir pb <//> nameVer <//> display name ++
                          ".haddock"))
    case enewPath of
        Left (e :: IOException) ->
            return () -- FIXME: log it with Shake.
        Right newPath ->
            liftIO
                (atomically $
                 modifyTVar haddockFiles $
                 M.insert nameVer newPath)

-- | Generate a flags string for the package plan.
planFlags :: PackagePlan -> String
planFlags plan = unwords $ map go $ M.toList (pcFlagOverrides (ppConstraints plan))
  where
    go (name',isOn) =
        concat
            [ if isOn
                  then ""
                  else "-"
            , T.unpack (unFlagName name')]

-- | Database location.
buildDatabase :: PerformBuild -> FilePattern
buildDatabase pb = FP.encodeString (pbInstallDest pb) <//> "pkgdb"

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
targetForDb' :: FilePath -> FilePath
targetForDb' shakeDir =
    shakeDir <//> "pkgdb"

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
