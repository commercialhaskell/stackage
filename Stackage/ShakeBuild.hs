{-# LANGUAGE ScopedTypeVariables #-}

-- | Build everything with Shake.

module Stackage.ShakeBuild where

import           Control.Monad
import           Stackage.BuildConstraints
import           Stackage.BuildPlan
import           Stackage.CheckBuildPlan
import           Stackage.PackageDescription
import           Stackage.PerformBuild (PerformBuild(..),copyBuiltInHaddocks,renameOrCopy)
import           Stackage.Prelude (unFlagName)

import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad hiding (forM_)
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Conduit.Process
import qualified Data.Conduit.Text as CT
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Version
import           Development.Shake hiding (doesFileExist,doesDirectoryExist)
import           Distribution.Compat.ReadP
import           Distribution.Package
import           Distribution.Package (PackageName)
import           Distribution.Text (display)
import           Distribution.Text (parse)
import qualified Filesystem.Path.CurrentOS as FP
import           System.Directory
import           System.Environment
import           System.Exit

-- | Run the shake builder.
performBuild :: PerformBuild -> IO ()
performBuild pb = do
    shakeDir <- fmap (<//> "shake/") (getCurrentDirectory >>= canonicalizePath)
    createDirectoryIfMissing True shakeDir
    haddockFiles <- liftIO (newTVarIO mempty)
    registerLock <- liftIO (newMVar ())
    cleanOldPackages pb shakeDir
    withArgs [] $
        shakeArgs
            shakeOptions
            { shakeFiles = shakeDir
            , shakeThreads = 2
            } $
        shakePlan haddockFiles registerLock pb shakeDir

-- | The complete build plan as far as Shake is concerned.
shakePlan :: TVar (Map String FilePath)
          -> MVar ()
          -> PerformBuild
          -> FilePath
          -> Rules ()
shakePlan haddockFiles registerLock pb shakeDir = do
    fetched <- target (targetForFetched shakeDir) $
               fetchedTarget shakeDir pb
    db <- target (targetForDb shakeDir) $
          databaseTarget shakeDir pb
    _ <- forM (mapMaybe (\p -> find ((==p) . fst) versionMappings) corePackages) $
         \(name,version) ->
              let fp = targetForPackage shakeDir name version
              in target fp (makeFile fp)
    packageTargets <- forM normalPackages $
                      \(name,plan) ->
                           target (targetForPackage shakeDir name (ppVersion plan)) $
                           do need [db, fetched]
                              packageTarget
                                  haddockFiles
                                  registerLock
                                  pb
                                  shakeDir
                                  name
                                  plan
    haddockTargets <- forM normalPackages $
                      \(name,plan) ->
                           target (targetForDocs shakeDir name (ppVersion plan)) $
                           do need [targetForPackage shakeDir name (ppVersion plan)]
                              packageDocs haddockFiles shakeDir pb plan name
    want haddockTargets
    where versionMappings = M.toList (M.map ppVersion (bpPackages (pbPlan pb)))
          corePackages = M.keys $ siCorePackages $ bpSystemInfo $ pbPlan pb
          normalPackages = filter (not . (`elem` corePackages) . fst) $
              M.toList $ bpPackages $ pbPlan pb

-- | Generate haddock docs for the package.
packageDocs :: TVar (Map String FilePath)
            -> FilePattern
            -> PerformBuild
            -> PackagePlan
            -> PackageName
            -> Action ()
packageDocs haddockFiles shakeDir pb plan name = do
    pwd <- liftIO getCurrentDirectory
    env <- liftIO (fmap (Env . (++ defaultEnv pwd)) getEnvironment)
    when
        (haddocksFlag /= Don'tBuild &&
         not (S.null $ sdModules $ ppDesc plan)) $
        generateHaddocks haddockFiles pb shakeDir (pkgDir shakeDir name version) env name version haddocksFlag
    makeFile (targetForDocs shakeDir name (ppVersion plan))
    where version = ppVersion plan
          haddocksFlag = pcHaddocks $ ppConstraints plan
          defaultEnv pwd = [( "HASKELL_PACKAGE_SANDBOX"
                            , pwd <//> buildDatabase shakeDir) | pbGlobalInstall pb]

-- | Initialize the database if there one needs to be, and in any case
-- create the target file.
databaseTarget :: FilePath -> PerformBuild -> Action ()
databaseTarget shakeDir pb = do
    if pbGlobalInstall pb
        then return ()
        else do
            liftIO (createDirectoryIfMissing True dir)
            liftIO (removeDirectoryRecursive dir)
            () <- cmd "ghc-pkg" "init" dir
            liftIO $ copyBuiltInHaddocks $ FP.decodeString $ pbDocDir shakeDir
    makeFile (targetForDb shakeDir)
    where dir = buildDatabase shakeDir

-- | Build, test and generate documentation for the package.
packageTarget :: TVar (Map String FilePath)
              -> MVar ()
              -> PerformBuild
              -> FilePath
              -> PackageName
              -> PackagePlan
              -> Action ()
packageTarget haddockFiles registerLock pb shakeDir name plan = do
    need $
        map (\(name,version) -> targetForPackage shakeDir name version) $
        mapMaybe (\p -> find ((==p) . fst) versionMappings) $
        filter (/= name) $
        M.keys $ M.filter libAndExe $ sdPackages $ ppDesc plan
    pwd <- liftIO getCurrentDirectory
    env <- liftIO (fmap (Env . (++ defaultEnv pwd)) getEnvironment)
    unpack shakeDir name version
    configure shakeDir dir env pb plan
    () <- cmd cwd env "cabal" "build" "--ghc-options=-O0"
    register dir env registerLock
    makeFile (targetForPackage shakeDir name version)
    where dir = pkgDir shakeDir name version
          version = ppVersion plan
          versionMappings = M.toList (M.map ppVersion (bpPackages (pbPlan pb)))
          cwd = Cwd dir
          defaultEnv pwd = [( "HASKELL_PACKAGE_SANDBOX"
                            , pwd <//> buildDatabase shakeDir) | pbGlobalInstall pb]

-- | Make sure all package archives have been fetched.
fetchedTarget :: FilePath -> PerformBuild -> Action ()
fetchedTarget shakeDir pb = do
    () <- cmd "cabal" "fetch" "--no-dependencies" $
          map
              (\(name,plan) ->
                    display name ++
                    "-" ++
                    display (ppVersion plan)) $
          M.toList $ bpPackages $ pbPlan pb
    makeFile (targetForFetched shakeDir)

-- | Unpack the package.
unpack :: FilePath -> PackageName -> Version -> Action ()
unpack shakeDir name version = do
    unpacked <- liftIO $
                doesFileExist $
                pkgDir shakeDir name version <//>
                display name ++
                ".cabal"
    unless unpacked $
        do liftIO $
               catch (removeDirectoryRecursive (pkgDir shakeDir name version)) $
               \(_ :: IOException) ->
                    return ()
           cmd
               (Cwd (shakeDir <//> "packages"))
               "cabal"
               "unpack"
               (nameVer name version)

-- | Configure the given package.
configure :: FilePath -> FilePath -> CmdOption -> PerformBuild -> PackagePlan -> Action ()
configure shakeDir pkgDir env pb plan = do
    pwd <- liftIO getCurrentDirectory
    cmd
        (Cwd pkgDir)
        env
        "cabal"
        "configure"
        (opts pwd)
    where opts pwd = [ "--package-db=clear"
                     , "--package-db=global"
                     , "--libdir=" ++ pbLibDir shakeDir
                     , "--bindir=" ++ pbBinDir shakeDir
                     , "--datadir=" ++ pbDataDir shakeDir
                     , "--docdir=" ++ pbDocDir shakeDir
                     , "--flags=" ++ planFlags plan] ++
              ["--package-db=" ++ buildDatabase shakeDir | not (pbGlobalInstall pb)]

-- | Register the package.
--
-- TODO: Do a mutex lock in here. Does Shake already support doing
-- this out of the box?
register :: FilePath -> CmdOption -> MVar () -> Action ()
register pkgDir env registerLock = do
    () <- cmd cwd env "cabal" "copy"
    -- FIXME:
    liftIO
        (takeMVar registerLock)
    () <- cmd cwd env "cabal" "register"
    liftIO (putMVar registerLock ())
    where cwd = Cwd pkgDir

-- | Generate haddocks for the package.
generateHaddocks :: TVar (Map String FilePath)
                 -> PerformBuild
                 -> FilePath
                 -> FilePath
                 -> CmdOption
                 -> PackageName
                 -> Version
                 -> TestState
                 -> Action ()
generateHaddocks haddockFiles pb shakeDir pkgDir env name version expected = do
    hfs <- liftIO $ readTVarIO haddockFiles
    exitCode <- cmd
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
    case (exitCode, expected) of
        (ExitSuccess,ExpectFailure) -> return () -- FIXME: warn.
        (ExitFailure{},ExpectSuccess) -> throw exitCode -- FIXME: report it
        _ -> return ()
    copy
    where ident = nameVer name version
          copy = do
              liftIO $
                  do let orig = pkgDocDir shakeDir name version
                     exists <- doesDirectoryExist orig
                     when exists $
                         renameOrCopy
                             (FP.decodeString orig)
                             (FP.decodeString
                                  (pbDocDir shakeDir <//> ident))
              enewPath <- liftIO $
                          try $
                          canonicalizePath
                              (pbDocDir shakeDir <//> ident <//> display name ++
                               ".haddock")
              case enewPath of
                  Left (e :: IOException) -> return () -- FIXME: log it with Shake.
                  Right newPath -> liftIO $
                      atomically $
                      modifyTVar haddockFiles $
                      M.insert (ident) newPath

-- | Generate a flags string for the package plan.
planFlags :: PackagePlan -> String
planFlags plan = unwords $
    map go $
    M.toList
        (pcFlagOverrides
             (ppConstraints plan))
    where go (name',isOn) = concat
                  [ if isOn
                        then ""
                        else "-"
                  , T.unpack (unFlagName name')]

-- | Database location.
buildDatabase :: FilePath -> FilePattern
buildDatabase shakeDir = shakeDir <//> "pkgdb"

-- | Print the name and version.
nameVer :: PackageName -> Version -> String
nameVer name version = display name ++ "-" ++ display version

-- | The directory for the package's docs.
pkgDocDir :: FilePath -> PackageName -> Version -> FilePath
pkgDocDir shakeDir name version = pkgDir shakeDir name version <//>
    "dist" <//>
    "doc" <//>
    "html" <//>
    (display name)

--  | The package directory.
pkgDir :: FilePath -> PackageName -> Version -> FilePath
pkgDir shakeDir name version = shakeDir <//> "packages" <//>
    (nameVer name version)

-- | Get the target file for confirming that all packages have been
-- pre-fetched.
targetForFetched :: FilePath -> FilePath
targetForFetched shakeDir =
    shakeDir <//> "packages-fetched"

-- | Get the target file for a package.
targetForPackage :: FilePath -> PackageName -> Version -> FilePath
targetForPackage shakeDir name version =
    shakeDir <//> "packages" <//> nameVer name version <//> "dist" <//> "shake-build"

-- | Get the target file for a package.
targetForDocs :: FilePath -> PackageName -> Version -> FilePath
targetForDocs shakeDir name version =
    shakeDir <//> "packages" <//> nameVer name version <//> "dist" <//> "shake-docs"

-- | Get a package database path.
targetForDb :: FilePath -> FilePath
targetForDb shakeDir =
    shakeDir <//> "pkgdb-initialized"

-- | Declare a target, returning the target name.
target :: FilePattern -> Action () -> Rules FilePattern
target name act = do
    name *> const act
    return name

-- | Make a file of this name.
makeFile :: FilePath -> Action ()
makeFile fp = liftIO $ writeFile fp ""

pbBinDir, pbLibDir, pbDataDir, pbDocDir :: FilePath -> FilePath
pbBinDir shakeDir = shakeDir <//> "bin"
pbLibDir shakeDir = shakeDir <//> "lib"
pbDataDir shakeDir = shakeDir <//> "share"
pbDocDir shakeDir = shakeDir <//> "doc"

-- | Reason for purging a package.
data PurgeReason
    = NoLongerIncluded
    | Replaced Version
    | Broken

-- | Clean up old versions of packages that are no longer in use.
cleanOldPackages :: PerformBuild -> FilePath -> IO ()
cleanOldPackages pb shakeDir = do
    putStrLn "Collecting garbage"
    pkgs <- getRegisteredPackages shakeDir
    forM_ pkgs $
        \(PackageIdentifier name version) ->
             case M.lookup name versions of
                 Just version'
                     | version' == version ->
                         return ()
                 Just newVersion -> purgePackage
                         shakeDir
                         name
                         version
                         (Replaced newVersion)
                 Nothing -> purgePackage shakeDir name version NoLongerIncluded
    broken <- getBrokenPackages shakeDir
    forM_
        broken
        (\(PackageIdentifier name version) ->
              purgePackage shakeDir name version Broken)
    where versions = (M.map ppVersion . bpPackages . pbPlan) pb

-- | Purge the given package and version.
purgePackage :: FilePath -> PackageName -> Version -> PurgeReason -> IO ()
purgePackage shakeDir name version reason = do
    putStr $ "Purging package: " ++ ident ++ " (" ++ showReason ++ ") ... "
    unregister
    delete
    putStrLn "done."
    where showReason =
              case reason of
                Replaced version' -> "replaced by " ++ ordinal ++ " " ++ display version'
                    where ordinal | version' > version = "newer"
                                  | otherwise          = "older"
                NoLongerIncluded -> "no longer included"
                Broken -> "broken"
          ident = nameVer name version
          unregister = do
              void (readProcessWithExitCode
                        "ghc-pkg"
                        ["unregister", "-f", buildDatabase shakeDir, "--force", ident]
                        "")
          delete = removeDirectoryRecursive $
              pkgDir shakeDir name version

-- | Get broken packages.
getBrokenPackages :: FilePath -> IO [PackageIdentifier]
getBrokenPackages shakeDir = do
    (_,ps) <- sourceProcessWithConsumer
                  (proc'
                       "ghc-pkg"
                       ["check", "--simple-output", "-f", buildDatabase shakeDir])
                  (CT.decodeUtf8 $= CT.lines $= CL.consume)
    return (mapMaybe parsePackageIdent (T.words (T.unlines ps)))

-- | Get available packages.
getRegisteredPackages :: FilePath -> IO [PackageIdentifier]
getRegisteredPackages shakeDir = do
    (_,ps) <- sourceProcessWithConsumer
                  (proc'
                       "ghc-pkg"
                       ["list", "--simple-output", "-f", buildDatabase shakeDir])
                  (CT.decodeUtf8 $= CT.lines $= CL.consume)
    return (mapMaybe parsePackageIdent (T.words (T.unlines ps)))

-- | Parse a package identifier: foo-1.2.3
parsePackageIdent :: Text -> Maybe PackageIdentifier
parsePackageIdent = fmap fst .
    listToMaybe .
    filter (null . snd) .
    readP_to_S parse . T.unpack

proc' = proc
