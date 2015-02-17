{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- | Build everything with Shake.

module Stackage.ShakeBuild where

import           Stackage.BuildConstraints
import           Stackage.BuildPlan
import           Stackage.CheckBuildPlan
import           Stackage.PackageDescription
import           Stackage.PerformBuild (PerformBuild(..),copyBuiltInHaddocks,renameOrCopy,copyDir)
import           Stackage.Prelude (unFlagName)

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
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
import           Data.Version
import           Development.Shake.FilePath
import           Distribution.Compat.ReadP
import           Distribution.Package
import           Distribution.Text (display)
import           Distribution.Text (parse)
import qualified Filesystem as FP
import           Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as FP
import           Prelude hiding (FilePath)
import           System.Environment
import           System.Exit

-- | Run the shake builder.
performBuild :: PerformBuild -> IO ()
performBuild pb' = do
    cur <- FP.getWorkingDirectory
    let shakeDir = cur <> "shake/"
    FP.createTree shakeDir
    haddockFiles <- liftIO (newTVarIO mempty)
    registerLock <- liftIO (newMVar ())
    pkgs <- getRegisteredPackages shakeDir
    let !pb = pb'
            { pbInstallDest = cur <> pbInstallDest pb'
            }
    cleanOldPackages pb shakeDir pkgs
    printNewPackages pb pkgs
    startShake 2 shakeDir (shakePlan haddockFiles registerLock pb shakeDir)

-- | The complete build plan as far as Shake is concerned.
shakePlan :: TVar (Map String FilePath) -> MVar () -> PerformBuild -> FilePath -> Rules ()
shakePlan haddockFiles registerLock pb shakeDir = do
    fetched <- target (targetForFetched shakeDir) $ fetchedTarget shakeDir pb
    db <- target (targetForDb shakeDir) $
          databaseTarget shakeDir pb
    _ <- forM (mapMaybe (\p -> find ((==p) . fst) versionMappings) corePackages) $
         \(name,version) ->
              let fp = targetForPackage shakeDir name version
              in target fp (makeTargetFile fp)
    packageTargets <-
        forM normalPackages $
        \(name,plan) ->
             target (targetForPackage shakeDir name (ppVersion plan)) $
             do need [db, fetched]
                packageTarget haddockFiles registerLock pb shakeDir name plan
    haddockTargets <-
        forM normalPackages $
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
packageDocs :: TVar (Map String FilePath) -> FilePath -> PerformBuild -> PackagePlan -> PackageName -> Action ()
packageDocs haddockFiles shakeDir pb plan name = do
    pwd <- liftIO FP.getWorkingDirectory
    env <- liftIO (fmap (Env . (++ defaultEnv pb shakeDir pwd)) getEnvironment)
    when (haddocksFlag /= Don'tBuild &&
          not (S.null $ sdModules $ ppDesc plan)) $
        generateHaddocks
            haddockFiles
            pb
            shakeDir
            (pkgDir shakeDir name version)
            env
            name
            version
            haddocksFlag
    makeTargetFile (targetForDocs shakeDir name (ppVersion plan))
    where version = ppVersion plan
          haddocksFlag = pcHaddocks $ ppConstraints plan

-- | Default environment for running commands.
defaultEnv :: PerformBuild -> FilePath -> FilePath -> [(String, String)]
defaultEnv pb shakeDir pwd =
    [( "HASKELL_PACKAGE_SANDBOX"
     , FP.encodeString (pwd <> buildDatabase shakeDir))
    | pbGlobalInstall pb]

-- | Initialize the database if there one needs to be, and in any case
-- create the target file.
databaseTarget :: FilePath -> PerformBuild -> Action ()
databaseTarget shakeDir pb = do
    if pbGlobalInstall pb
        then return ()
        else do
            liftIO (FP.removeTree dir)
            liftIO (FP.createTree dir)
            () <- cmd "ghc-pkg" "init" (FP.encodeString dir)
            liftIO $ copyBuiltInHaddocks $ pbDocDir pb
    makeTargetFile (targetForDb shakeDir)
    where dir = buildDatabase shakeDir

-- | Build, test and generate documentation for the package.
packageTarget :: TVar (Map String FilePath) -> MVar () -> PerformBuild -> FilePath -> PackageName -> PackagePlan -> Action ()
packageTarget haddockFiles registerLock pb shakeDir name plan = do
    need $
        map (\(name,version) -> targetForPackage shakeDir name version) $
        mapMaybe (\p -> find ((==p) . fst) versionMappings) $
        filter (/= name) $
        M.keys $ M.filter libAndExe $ sdPackages $ ppDesc plan
    pwd <- liftIO FP.getWorkingDirectory
    env <- liftIO (fmap (Env . (++ defaultEnv pb shakeDir pwd)) getEnvironment)
    unpack shakeDir name version
    configure shakeDir dir env pb plan
    () <- cmd cwd env "cabal" "build" "--ghc-options=-O0"
    register dir env registerLock
    makeTargetFile (targetForPackage shakeDir name version)
    where dir = pkgDir shakeDir name version
          version = ppVersion plan
          versionMappings = M.toList (M.map ppVersion (bpPackages (pbPlan pb)))
          cwd = Cwd (FP.encodeString dir)

-- | Make sure all package archives have been fetched.
fetchedTarget :: FilePath -> PerformBuild -> Action ()
fetchedTarget shakeDir pb = do
    () <- cmd "cabal" "fetch" "--no-dependencies" $
          map
              (\(name,plan) -> display name ++ "-" ++ display (ppVersion plan)) $
          M.toList $ bpPackages $ pbPlan pb
    makeTargetFile (targetForFetched shakeDir)

-- | Unpack the package.
unpack :: FilePath -> PackageName -> Version -> Action ()
unpack shakeDir name version = do
    unpacked <- liftIO $ FP.isFile $
        pkgDir shakeDir name version <>
        FP.decodeString
            (display name ++ ".cabal")
    unless unpacked $
        do liftIO $ catch (FP.removeTree (pkgDir shakeDir name version)) $
               \(_ :: IOException) -> return ()
           cmd
               (Cwd (FP.encodeString (shakeDir <> "packages")))
               "cabal"
               "unpack"
               (nameVer name version)

-- | Configure the given package.
configure :: FilePath -> FilePath -> CmdOption -> PerformBuild -> PackagePlan -> Action ()
configure shakeDir pkgDir env pb plan = do
    pwd <- liftIO FP.getWorkingDirectory
    cmd (Cwd (FP.encodeString pkgDir)) env "cabal" "configure" (opts pwd)
    where
        opts pwd =
            [ "--package-db=clear"
            , "--package-db=global"
            , "--libdir=" ++ FP.encodeString (pbLibDir pb)
            , "--bindir=" ++ FP.encodeString (pbBinDir pb)
            , "--datadir=" ++ FP.encodeString (pbDataDir pb)
            , "--docdir=" ++ FP.encodeString (pbDocDir pb)
            , "--flags=" ++ planFlags plan] ++
            ["--package-db=" ++ FP.encodeString (buildDatabase shakeDir) | not (pbGlobalInstall pb)]

-- | Register the package.
--
-- TODO: Do a mutex lock in here. Does Shake already support doing
-- this out of the box?
register :: FilePath -> CmdOption -> MVar () -> Action ()
register pkgDir env registerLock = do
    () <- cmd cwd env "cabal" "copy"
    liftIO (takeMVar registerLock)
    () <- cmd cwd env "cabal" "register"
    liftIO (putMVar registerLock ())
    where cwd = Cwd (FP.encodeString pkgDir)

-- | Generate haddocks for the package.
generateHaddocks :: TVar (Map String FilePath) -> PerformBuild -> FilePath -> FilePath -> CmdOption -> PackageName -> Version -> TestState -> Action ()
generateHaddocks haddockFiles pb shakeDir pkgDir env name version expected = do
    hfs <- liftIO $ readTVarIO haddockFiles
    exitCode <-
        cmd
            (Cwd (FP.encodeString pkgDir))
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
                           , FP.encodeString hf])
                 (M.toList hfs))
    case (exitCode, expected) of
        (ExitSuccess,ExpectFailure) -> return () -- FIXME: warn.
        (ExitFailure{},ExpectSuccess) -> throw exitCode -- FIXME: report it
        _ -> return ()
    copy
    where
        ident = nameVer name version
        copy = do
            liftIO $
                do let orig = pkgDocDir shakeDir name version
                   exists <- FP.isDirectory orig
                   when exists $
                       renameOrCopy
                           orig
                           (pbDocDir pb <> FP.decodeString ident)
            enewPath <-
                liftIO $
                try $
                FP.canonicalizePath
                    (pbDocDir pb <> FP.decodeString ident <>
                     FP.decodeString (display name ++ ".haddock"))
            case enewPath of
                Left (e :: IOException) -> return () -- FIXME: log it with Shake.
                Right newPath -> liftIO $ atomically $ modifyTVar haddockFiles $
                    M.insert (ident) newPath

-- | Generate a flags string for the package plan.
planFlags :: PackagePlan -> String
planFlags plan = unwords $
    map go $ M.toList (pcFlagOverrides (ppConstraints plan))
    where go (name',isOn) = concat
                  [ if isOn then "" else "-" , T.unpack (unFlagName name')]

-- | Database location.
buildDatabase :: FilePath -> FilePath
buildDatabase shakeDir = shakeDir <> "pkgdb"

-- | Print the name and version.
nameVer :: PackageName -> Version -> String
nameVer name version = display name ++ "-" ++ display version

-- | The directory for the package's docs.
pkgDocDir :: FilePath -> PackageName -> Version -> FilePath
pkgDocDir shakeDir name version = pkgDir shakeDir name version <>
    "dist" <>
    "doc" <>
    "html" <>
    (FP.decodeString (display name))

--  | The package directory.
pkgDir :: FilePath -> PackageName -> Version -> FilePath
pkgDir shakeDir name version = shakeDir <> "packages" <>
    (FP.decodeString (nameVer name version))

-- | Get the target file for confirming that all packages have been
-- pre-fetched.
targetForFetched :: FilePath -> Target
targetForFetched shakeDir =
    Target (shakeDir <> "packages-fetched")

-- | Get the target file for a package.
targetForPackage :: FilePath -> PackageName -> Version -> Target
targetForPackage shakeDir name version = Target $
    shakeDir <> "packages" <>
    FP.decodeString
        (nameVer name version) <>
    "dist" <>
    "shake-build"

-- | Get the target file for a package.
targetForDocs :: FilePath -> PackageName -> Version -> Target
targetForDocs shakeDir name version = Target $
    shakeDir <> "packages" <>
    FP.decodeString
        (nameVer name version) <>
    "dist" <>
    "shake-docs"

-- | Target for the complete, copied build under builds/date/.
targetForBuild :: PerformBuild -> Target
targetForBuild pb =  Target $ (pbInstallDest pb) <> "shake-built"

-- | Get a package database path.
targetForDb :: FilePath -> Target
targetForDb shakeDir =
    Target $ shakeDir <> "pkgdb-initialized"

-- | Make a file of this name.
makeTargetFile :: Target -> Action ()
makeTargetFile fp = liftIO $ FP.writeFile (unTarget fp) ""

pbBinDir, pbLibDir, pbDataDir, pbDocDir :: PerformBuild -> FilePath
pbBinDir root =  (pbInstallDest root) <> "bin"
pbLibDir root =  (pbInstallDest root) <> "lib"
pbDataDir root =  (pbInstallDest root) <> "share"
pbDocDir root =  (pbInstallDest root) <> "doc"

-- | Reason for purging a package.
data PurgeReason
    = NoLongerIncluded
    | Replaced Version
    | Broken

-- | Print the new packages.
printNewPackages :: PerformBuild -> [PackageIdentifier] -> IO (Map PackageName Version)
printNewPackages pb pkgs = do
    unless
        (M.null new)
        (do putStrLn
                ("There are " ++
                 show (M.size new) ++
                 " packages to build and install: ")
            forM_
                (take maxDisplay (M.toList new))
                (\(name,ver) ->
                      putStrLn (display name))
            when (M.size new > maxDisplay)
                 (putStrLn ("And " ++ show (M.size new - maxDisplay) ++ " more.")))
    return new
    where maxDisplay = 10
          new = M.filterWithKey
                  (\name ver ->
                        isNothing (find ((== name) . pkgName) pkgs))
                  versions
          versions = (M.map ppVersion .
                      M.filter (not . S.null . sdModules . ppDesc) .
                      bpPackages . pbPlan) pb

-- | Clean up old versions of packages that are no longer in use.
cleanOldPackages :: PerformBuild -> FilePath -> [PackageIdentifier] -> IO ()
cleanOldPackages pb shakeDir pkgs = do
    putStrLn "Collecting garbage"
    pkgs <- getRegisteredPackages shakeDir
    let toRemove = mapMaybe
                (\(PackageIdentifier name version) ->
                      case M.lookup name versions of
                          Just version'
                              | version' == version ->
                                  Nothing
                          Just newVersion -> Just
                                  (name, version, (Replaced newVersion))
                          Nothing -> Just (name, version, NoLongerIncluded))
                pkgs
    unless (null toRemove)
           (putStrLn ("There are " ++ show (length toRemove) ++ " packages to be purged."))
    when (length toRemove > 0)
         (do putStrLn "Waiting 3 seconds before proceeding to remove ..."
             threadDelay (1000 * 1000 * 3))
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
    unless (null broken)
           (putStrLn ("There are " ++ show (length broken) ++ " broken packages to be purged."))
    when (length broken > 0)
         (do putStrLn "Waiting 3 seconds before proceeding to remove ..."
             threadDelay (1000 * 1000 * 3))
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
                        ["unregister", "-f", FP.encodeString (buildDatabase shakeDir), "--force", ident]
                        "")
          delete = FP.removeTree $
              pkgDir shakeDir name version

-- | Get broken packages.
getBrokenPackages :: FilePath -> IO [PackageIdentifier]
getBrokenPackages shakeDir = do
    (_,ps) <- sourceProcessWithConsumer
                  (proc
                       "ghc-pkg"
                       ["check", "--simple-output", "-f", FP.encodeString (buildDatabase shakeDir)])
                  (CT.decodeUtf8 $= CT.lines $= CL.consume)
    return (mapMaybe parsePackageIdent (T.words (T.unlines ps)))

-- | Get available packages.
getRegisteredPackages :: FilePath -> IO [PackageIdentifier]
getRegisteredPackages shakeDir = do
    (_,ps) <- sourceProcessWithConsumer
                  (proc
                       "ghc-pkg"
                       ["list", "--simple-output", "-f", FP.encodeString (buildDatabase shakeDir)])
                  (CT.decodeUtf8 $= CT.lines $= CL.consume)
    return (mapMaybe parsePackageIdent (T.words (T.unlines ps)))

-- | Parse a package identifier: foo-1.2.3
parsePackageIdent :: Text -> Maybe PackageIdentifier
parsePackageIdent = fmap fst .
    listToMaybe .
    filter (null . snd) .
    readP_to_S parse . T.unpack
