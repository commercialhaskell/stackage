{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Build everything with Shake.

module Stackage.ShakeBuild where

import           Stackage.BuildConstraints
import           Stackage.BuildPlan
import           Stackage.CheckBuildPlan
import           Stackage.PackageDescription
import           Stackage.PerformBuild (PerformBuild(..),copyBuiltInHaddocks,renameOrCopy)
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
import           Development.Shake.FilePath hiding (Env)
import qualified Development.Shake.FilePath as Shake
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

data Env = Env
    {envCur      :: FilePath
    ,envShake :: FilePath
    ,envHadLock  :: TVar (Map String FilePath)
    ,envRegLock  :: MVar ()
    ,envPB       :: PerformBuild
    ,envRegistered :: [PackageIdentifier]
    }

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
        !env = Env
            { envCur = cur
            , envShake = shakeDir
            , envHadLock = haddockFiles
            , envRegLock = registerLock
            , envPB = pb
            , envRegistered = pkgs
            }
    cleanOldPackages env
    printNewPackages env
    startShake 2 shakeDir (shakePlan env)

-- | The complete build plan as far as Shake is concerned.
shakePlan :: Env -> Rules ()
shakePlan env@Env{..} = do
    fetched <- target (targetForFetched env) $ fetchedTarget env
    db <- target (targetForDb env) $ databaseTarget env
    void $ forM (mapMaybe (\p -> find ((==p) . fst) versionMappings) corePackages) $
        \(name,version) ->
           let fp = targetForPackage envShake name version
           in target fp (makeTargetFile fp)
    void $ forM normalPackages $
        \(name,plan) ->
             target (targetForPackage envShake name (ppVersion plan)) $
             do need [db, fetched]
                packageTarget env name plan
    haddockTargets <-
        forM normalPackages $
        \(name,plan) ->
             target (targetForDocs envShake name (ppVersion plan)) $
             do need [targetForPackage envShake name (ppVersion plan)]
                packageDocs env plan name
    want haddockTargets
    where versionMappings = M.toList (M.map ppVersion (bpPackages (pbPlan envPB)))
          corePackages = M.keys $ siCorePackages $ bpSystemInfo $ pbPlan envPB
          normalPackages = filter (not . (`elem` corePackages) . fst) $
              M.toList $ bpPackages $ pbPlan envPB

-- | Generate haddock docs for the package.
packageDocs :: Env -> PackagePlan -> PackageName -> Action ()
packageDocs env@Env{..} plan name = do
    pwd <- liftIO FP.getWorkingDirectory
    envmap <- liftIO (fmap (Shake.Env . (++ defaultEnv envPB envShake pwd)) getEnvironment)
    when (haddocksFlag /= Don'tBuild &&
          not (S.null $ sdModules $ ppDesc plan)) $
        generateHaddocks
            env
            (pkgDir env name version)
            envmap
            name
            version
            haddocksFlag
    makeTargetFile (targetForDocs envShake name (ppVersion plan))
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
databaseTarget :: Env -> Action ()
databaseTarget env  = do
    if pbGlobalInstall (envPB env)
        then return ()
        else do
            liftIO (FP.removeTree dir)
            liftIO (FP.createTree dir)
            () <- cmd "ghc-pkg" "init" (FP.encodeString dir)
            liftIO $ copyBuiltInHaddocks $ pbDocDir (envPB env)
    makeTargetFile (targetForDb env)
    where dir = buildDatabase (envShake env)

-- | Build, test and generate documentation for the package.
packageTarget :: Env -> PackageName -> PackagePlan -> Action ()
packageTarget env@Env{..} name plan = do
    need $
        map (\(pname,pver) -> targetForPackage envShake pname pver) $
        mapMaybe (\p -> find ((==p) . fst) versionMappings) $
        filter (/= name) $
        M.keys $ M.filter libAndExe $ sdPackages $ ppDesc plan
    pwd <- liftIO FP.getWorkingDirectory
    envmap <- liftIO (fmap (Shake.Env . (++ defaultEnv envPB envShake pwd)) getEnvironment)
    unpack env name version
    configure env dir envmap plan
    () <- cmd cwd envmap "cabal" "build" "--ghc-options=-O0"
    register dir envmap envRegLock
    makeTargetFile (targetForPackage envShake name version)
    where dir = pkgDir env name version
          version = ppVersion plan
          versionMappings = M.toList (M.map ppVersion (bpPackages (pbPlan envPB)))
          cwd = Cwd (FP.encodeString dir)

-- | Make sure all package archives have been fetched.
fetchedTarget :: Env -> Action ()
fetchedTarget env@Env{..} = do
    () <- cmd "cabal" "fetch" "--no-dependencies" $
          map
              (\(name,plan) -> display name ++ "-" ++ display (ppVersion plan)) $
          M.toList $ bpPackages $ pbPlan envPB
    makeTargetFile (targetForFetched env)

-- | Unpack the package.
unpack :: Env -> PackageName -> Version -> Action ()
unpack env@Env{..} name version = do
    unpacked <- liftIO $ FP.isFile $
        pkgDir env name version <>
        FP.decodeString
            (display name ++ ".cabal")
    unless unpacked $
        do liftIO $ catch (FP.removeTree (pkgDir env name version)) $
               \(_ :: IOException) -> return ()
           cmd
               (Cwd (FP.encodeString (envShake <> "packages")))
               "cabal"
               "unpack"
               (nameVer name version)

-- | Configure the given package.
configure :: Env -> FilePath -> CmdOption -> PackagePlan -> Action ()
configure Env{..} pdir env plan =
    cmd (Cwd (FP.encodeString pdir)) env "cabal" "configure" opts
    where
        opts =
            [ "--package-db=clear"
            , "--package-db=global"
            , "--libdir=" ++ FP.encodeString (pbLibDir envPB)
            , "--bindir=" ++ FP.encodeString (pbBinDir envPB)
            , "--datadir=" ++ FP.encodeString (pbDataDir envPB)
            , "--docdir=" ++ FP.encodeString (pbDocDir envPB)
            , "--flags=" ++ planFlags plan] ++
            ["--package-db=" ++ FP.encodeString (buildDatabase envShake)
            | not (pbGlobalInstall envPB)]

-- | Register the package.
--
-- TODO: Do a mutex lock in here. Does Shake already support doing
-- this out of the box?
register :: FilePath -> CmdOption -> MVar () -> Action ()
register pdir env registerLock = do
    () <- cmd cwd env "cabal" "copy"
    liftIO (takeMVar registerLock)
    () <- cmd cwd env "cabal" "register"
    liftIO (putMVar registerLock ())
    where cwd = Cwd (FP.encodeString pdir)

-- | Generate haddocks for the package.
generateHaddocks :: Env -> FilePath -> CmdOption -> PackageName -> Version -> TestState -> Action ()
generateHaddocks env@Env{..} pdir envmap name version expected = do
    hfs <- liftIO $ readTVarIO envHadLock
    exitCode <-
        cmd
            (Cwd (FP.encodeString pdir))
            envmap
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
                do let orig = pkgDocDir env name version
                   exists <- FP.isDirectory orig
                   when exists $
                       renameOrCopy
                           orig
                           (pbDocDir envPB <> FP.decodeString ident)
            enewPath <-
                liftIO $
                try $
                FP.canonicalizePath
                    (pbDocDir envPB <> FP.decodeString ident <>
                     FP.decodeString (display name ++ ".haddock"))
            case enewPath of
                Left (_ :: IOException) -> return () -- FIXME: log it with Shake.
                Right newPath -> liftIO $ atomically $ modifyTVar envHadLock $
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
pkgDocDir :: Env -> PackageName -> Version -> FilePath
pkgDocDir env@Env{..} name version = pkgDir env name version <>
    "dist" <>
    "doc" <>
    "html" <>
    (FP.decodeString (display name))

--  | The package directory.
pkgDir :: Env -> PackageName -> Version -> FilePath
pkgDir Env{..} name version = envShake <> "packages" <>
    (FP.decodeString (nameVer name version))

-- | Get the target file for confirming that all packages have been
-- pre-fetched.
targetForFetched :: Env -> Target
targetForFetched Env{..} =
    Target (envShake <> "packages-fetched")

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
targetForDb :: Env -> Target
targetForDb Env{..} =
    Target $ envShake <> "pkgdb-initialized"

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
printNewPackages :: Env -> IO ()
printNewPackages Env{..} = do
    unless
        (M.null new)
        (do putStrLn
                ("There are " ++
                 show (M.size new) ++
                 " packages to build and install: ")
            forM_
                (map fst (take maxDisplay (M.toList new)))
                (putStrLn . display)
            when (M.size new > maxDisplay)
                 (putStrLn ("And " ++ show (M.size new - maxDisplay) ++ " more.")))
    where maxDisplay = 10
          new = M.filterWithKey
                  (\name _ ->
                        isNothing (find ((== name) . pkgName) envRegistered))
                  versions
          versions = (M.map ppVersion .
                      M.filter (not . S.null . sdModules . ppDesc) .
                      bpPackages . pbPlan) envPB

-- | Clean up old versions of packages that are no longer in use.
cleanOldPackages :: Env -> IO ()
cleanOldPackages env@Env{..} = do
    putStrLn "Collecting garbage"
    pkgs <- getRegisteredPackages envShake
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
                         env
                         name
                         version
                         (Replaced newVersion)
                 Nothing -> purgePackage env name version NoLongerIncluded
    broken <- getBrokenPackages envShake
    unless (null broken)
           (putStrLn ("There are " ++ show (length broken) ++ " broken packages to be purged."))
    when (length broken > 0)
         (do putStrLn "Waiting 3 seconds before proceeding to remove ..."
             threadDelay (1000 * 1000 * 3))
    forM_
        broken
        (\(PackageIdentifier name version) ->
              purgePackage env name version Broken)
    where versions = (M.map ppVersion . bpPackages . pbPlan) envPB

-- | Purge the given package and version.
purgePackage :: Env -> PackageName -> Version -> PurgeReason -> IO ()
purgePackage env name version reason = do
    putStr $ "Purging package: " ++ ident ++ " (" ++ showReason ++ ") ... "
    unregister
    remove
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
                        ["unregister", "-f", FP.encodeString (buildDatabase (envShake env)), "--force", ident]
                        "")
          remove = FP.removeTree $
              pkgDir env name version

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
