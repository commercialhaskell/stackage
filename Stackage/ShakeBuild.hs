{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Build everything with Shake.

module Stackage.ShakeBuild (performBuild) where

import           Stackage.BuildConstraints
import           Stackage.BuildPlan
import           Stackage.CheckBuildPlan
import           Stackage.GhcPkg
import           Stackage.PackageDescription
import           Stackage.PerformBuild (PerformBuild(..),copyBuiltInHaddocks,renameOrCopy)
import           Stackage.Prelude (unFlagName,unExeName)

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as S
import           Data.Streaming.Process
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Version
import           Development.Shake.FilePath hiding (Env)
import           Distribution.Package
import           Distribution.Text (display)
import qualified Filesystem as FP
import           Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as FP
import           Prelude hiding (log,FilePath)
import           System.Environment
import           System.Exit
import           System.IO (withBinaryFile,IOMode(AppendMode))

-- | Reader environment used generally throughout the build process.
data Env = Env
    {envCur        :: FilePath                   -- ^ Current directory.
    ,envShake      :: FilePath                   -- ^ Shake directory.
    ,envHaddocks   :: TVar (Map String FilePath) -- ^ Haddock files.
    ,envRegLock    :: MVar ()                    -- ^ Package registering lock.
    ,envPB         :: PerformBuild               -- ^ Build perform settings.
    ,envRegistered :: [PackageIdentifier]        -- ^ Registered packages.
    ,envMsgLock    :: MVar ()                    -- ^ A lock for printing to the log.
    ,envStatus     :: TVar ExitCode
    }

--------------------------------------------------------------------------------
-- Main entry point

-- | Run the shake builder.
performBuild :: PerformBuild -> IO ()
performBuild pb' = do
    num <- getNumCapabilities
    cur <- FP.getWorkingDirectory
    let shakeDir = cur <> "shake/"
    FP.createTree shakeDir
    FP.createTree (buildDatabase pb')
    haddockFiles <- newTVarIO mempty
    registerLock <- newMVar ()
    let !pb = pb'
             { pbInstallDest = cur <> pbInstallDest pb'
             }
    pkgs <- getRegisteredPackages (buildDatabase pb)
    msgLock <- newMVar ()
    status <- newTVarIO ExitSuccess
    let !env = Env
            { envCur = cur
            , envShake = shakeDir
            , envHaddocks = haddockFiles
            , envRegLock = registerLock
            , envPB = pb
            , envRegistered = pkgs
            , envMsgLock = msgLock
            , envStatus = status
            }
    checkBuildTools env
    cleanOldPackages env
    printNewPackages env
    startShake num shakeDir (shakePlan env)
    st <- readTVarIO status
    case st of
      ExitSuccess -> return ()
      _  -> throw st

--------------------------------------------------------------------------------
-- The whole Shake plan

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

--------------------------------------------------------------------------------
-- Target file paths

-- | Get the target file for confirming that all packages have been
-- pre-fetched.
targetForFetched :: Env -> Target
targetForFetched Env{..} = Target (envShake <> "packages-fetched")

-- | Get the target file for a package.
targetForPackage :: FilePath -> PackageName -> Version -> Target
targetForPackage shakeDir name version = Target $
    shakeDir <> "packages" <>
    FP.decodeString (nameVer name version)
    <> "dist" <> "shake-build"

-- | Get the target file for a package.
targetForDocs :: FilePath -> PackageName -> Version -> Target
targetForDocs shakeDir name version = Target $
    shakeDir <> "packages" <>
    FP.decodeString
        (nameVer name version) <>
    "dist" <> "shake-docs"

-- | Get a package database path.
targetForDb :: Env -> Target
targetForDb Env{..} = Target $ (pbInstallDest envPB) <> "pkgdb-initialized"

--------------------------------------------------------------------------------
-- Locations, names and environments used. Just to avoid "magic
-- strings".

-- | Print the name and version.
nameVer :: PackageName -> Version -> String
nameVer name version = display name ++ "-" ++ display version

-- | Default environment for running commands.
defaultEnv :: PerformBuild -> FilePath -> [(String, String)]
defaultEnv pb pwd =
    [( "HASKELL_PACKAGE_SANDBOX"
     , FP.encodeString (pwd <> buildDatabase pb))
    | pbGlobalInstall pb]

-- | Database location.
buildDatabase :: PerformBuild -> FilePath
buildDatabase pb = (pbInstallDest pb) <> "pkgdb"

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

--  | The package directory.
pkgLogFile :: Env -> PackageName -> Version -> FilePath
pkgLogFile env@Env{..} name version = pkgDir env name version <>
    "dist" <> "stackage-log.txt"

-- | Installation paths.
pbBinDir, pbLibDir, pbDataDir, pbDocDir :: PerformBuild -> FilePath
pbBinDir root =  (pbInstallDest root) <> "bin"
pbLibDir root =  (pbInstallDest root) <> "lib"
pbDataDir root =  (pbInstallDest root) <> "share"
pbDocDir root =  (pbInstallDest root) <> "doc"

--------------------------------------------------------------------------------
-- Pre-build messages

-- | Print the new packages.
printNewPackages :: Env -> IO ()
printNewPackages env@Env{..} = do
    unless
        (M.null new)
        (do logLn
                env
                Normal
                ("There are " ++
                 show (M.size new) ++
                 " packages to build and install.")
            forM_
                (map fst (take maxDisplay (M.toList new)))
                (logLn env Verbose . display)
            when
                (M.size new > maxDisplay)
                (logLn
                       env
                       Verbose
                       ("And " ++
                        show (M.size new - maxDisplay) ++
                        " more.")))
    where maxDisplay = 10
          new = newPackages env

-- | Get new packages from the env.
newPackages :: Env -> Map PackageName Version
newPackages Env{..} = new
  where new = M.filterWithKey
                (\name _ ->
                      isNothing (find ((== name) . pkgName) envRegistered))
                versions
        versions = (M.map ppVersion .
                    M.filter (not . S.null . sdModules . ppDesc) .
                    bpPackages . pbPlan) envPB

--------------------------------------------------------------------------------
-- Checking for build tools

-- | Check that all build tools are available.
-- https://github.com/jgm/zip-archive/issues/23
checkBuildTools :: Env -> IO ()
checkBuildTools env@Env{..} =
    forM_ normalPackages
          (\(pname,plan) -> mapM_ (checkTool pname) (M.keys (sdTools (ppDesc plan))))
  where normalPackages = filter (not . (`elem` corePackages) . fst) $
            M.toList $ bpPackages $ pbPlan envPB
          where corePackages = M.keys $ siCorePackages $ bpSystemInfo $ pbPlan envPB
        checkTool pname name =
            case M.lookup name (makeToolMap (bpPackages (pbPlan envPB))) of
              Nothing
                  | not (isCoreExe name) ->
                      logLn env Normal ("Warning: No executable " <>
                                      T.unpack (unExeName name) <>
                                      " for " <> display pname)

              Just _
                  -> return ()
              _ -> return ()
        isCoreExe = (`S.member` siCoreExecutables (bpSystemInfo (pbPlan envPB)))

--------------------------------------------------------------------------------
-- Clean/purging of old packages

-- | Reason for purging a package.
data PurgeReason
    = NoLongerIncluded
    | Replaced Version
    | Broken

-- | Clean up old versions of packages that are no longer in use.
cleanOldPackages :: Env -> IO ()
cleanOldPackages env@Env{..} = do
    logLn env Verbose "Collecting garbage"
    pkgs <- getRegisteredPackages (buildDatabase envPB)
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
           (logLn env Verbose ("There are " ++ show (length toRemove)
                               ++ " packages to be purged."))
    when (length toRemove > 0)
         (do logLn env Verbose "Waiting 3 seconds before proceeding to remove ..."
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
    broken <- getBrokenPackages (buildDatabase envPB)
    unless (null broken)
           (logLn env Verbose ("There are " ++ show (length broken)
                               ++ " broken packages to be purged."))
    when (length broken > 0)
         (do logLn env Verbose "Waiting 3 seconds before proceeding to remove ..."
             threadDelay (1000 * 1000 * 3))
    forM_
        broken
        (\(PackageIdentifier name version) ->
              purgePackage env name version Broken)
    where versions = (M.map ppVersion . bpPackages . pbPlan) envPB

-- | Purge the given package and version.
purgePackage :: Env -> PackageName -> Version -> PurgeReason -> IO ()
purgePackage env name version reason = do
    log env Normal $ "Purging package: " ++ ident ++ " (" ++ showReason ++ ") ... "
    unregisterPackage (buildDatabase (envPB env)) name
    remove
    logLn env Normal "done."
    where showReason =
              case reason of
                Replaced version' -> "replaced by " ++ ordinal ++ " " ++ display version'
                    where ordinal | version' > version = "newer"
                                  | otherwise          = "older"
                NoLongerIncluded -> "no longer included"
                Broken -> "broken"
          ident = nameVer name version
          remove = FP.removeTree $
              pkgDir env name version

--------------------------------------------------------------------------------
-- Target actions

-- | Initialize the database if there one needs to be, and in any case
-- create the target file.
databaseTarget :: Env -> Action ()
databaseTarget env  = do
    if pbGlobalInstall (envPB env)
        then return ()
        else do
            liftIO (FP.createTree dir)
            liftIO (FP.removeTree dir)
            () <- cmd "ghc-pkg" "init" (FP.encodeString dir)
            liftIO $ copyBuiltInHaddocks $ pbDocDir (envPB env)
    makeTargetFile (targetForDb env)
    where dir = buildDatabase (envPB env)

-- | Generate haddock docs for the package.
packageDocs :: Env -> PackagePlan -> PackageName -> Action ()
packageDocs env@Env{..} plan name = do
    when (haddocksFlag /= Don'tBuild &&
          not (S.null $ sdModules $ ppDesc plan)) $
        generateHaddocks
            env
            (pkgLogFile env name version)
            (pkgDir env name version)
            name
            version
            haddocksFlag
    makeTargetFile (targetForDocs envShake name (ppVersion plan))
    where version = ppVersion plan
          haddocksFlag = pcHaddocks $ ppConstraints plan

-- | Build, test and generate documentation for the package.
packageTarget :: Env -> PackageName -> PackagePlan -> Action ()
packageTarget env@Env{..} name plan = do
    need $
        map (\(pname,pver) -> targetForPackage envShake pname pver) $
        mapMaybe (\p -> find ((==p) . fst) versionMappings) $
        filter (/= name) $
        M.keys $ M.filter libAndExe $ sdPackages $ ppDesc plan
    unpack env name version
    liftIO (do exists <- FP.isFile logFile
               when exists (FP.removeFile logFile))
    prefix <- packageCmdPrefix name
    cabal env Verbose prefix logFile dir ["clean"]
    configure env name logFile dir plan False
    let pkgCabal :: (MonadIO m) => Verbosity -> [String] -> m ()
        pkgCabal verbosity = succeed . cabal env verbosity prefix logFile dir
    pkgCabal Normal ["build","--ghc-options=" <> pbGhcOptions envPB]
    when (pbEnableTests envPB && pcTests (ppConstraints plan) /= Don'tBuild)
         (do configure env name logFile dir plan True
             result <- cabal env Normal prefix logFile dir ["test"]
             case (result,pcTests (ppConstraints plan)) of
               (ExitFailure{},ExpectSuccess) ->
                   do logLn env Normal (prefix <> "TEST SUITE FAILED")
                      failed env result
               (ExitSuccess,ExpectFailure) ->
                   logLn env Normal (prefix <> "Unexpected test suite success!")
               _ -> return ())
    pkgCabal Verbose ["copy"]
    liftIO (withMVar envRegLock
                     (const (pkgCabal Verbose ["register"])))
    makeTargetFile (targetForPackage envShake name version)
    where logFile = pkgLogFile env name version
          dir = pkgDir env name version
          version = ppVersion plan
          versionMappings = M.toList (M.map ppVersion (bpPackages (pbPlan envPB)))

-- | Make sure all package archives have been fetched.
fetchedTarget :: Env -> Action ()
fetchedTarget env@Env{..} = do
    () <- cmd "cabal" "fetch" "--no-dependencies" $
          map
              (\(name,plan) -> display name ++ "-" ++ display (ppVersion plan)) $
          M.toList $ bpPackages $ pbPlan envPB
    makeTargetFile (targetForFetched env)

--------------------------------------------------------------------------------
-- Package actions

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
               "-v0"

-- | Configure the given package.
configure :: Env -> PackageName -> FilePath -> FilePath -> PackagePlan -> Bool -> Action ()
configure env@Env{..} name logfile pdir plan enableTests =
    do prefix <- packageCmdPrefix name
       succeed (cabal env Verbose prefix logfile pdir ("configure" : opts))
    where
        opts =
            [ "--package-db=clear"
            , "--package-db=global"
            , "--libdir=" ++ FP.encodeString (pbLibDir envPB)
            , "--bindir=" ++ FP.encodeString (pbBinDir envPB)
            , "--datadir=" ++ FP.encodeString (pbDataDir envPB)
            , "--docdir=" ++ FP.encodeString (pbDocDir envPB)
            , "--flags=" ++ planFlags] ++
            ["--package-db=" ++ FP.encodeString (buildDatabase envPB)
            | not (pbGlobalInstall envPB)] ++
            ["--enable-tests" | enableTests]
        planFlags = unwords $
           map go $ M.toList (pcFlagOverrides (ppConstraints plan))
           where go (name',isOn) = concat
                         [ if isOn then "" else "-" , T.unpack (unFlagName name')]

-- | Generate haddocks for the package.
generateHaddocks :: Env -> FilePath -> FilePath -> PackageName -> Version -> TestState -> Action ()
generateHaddocks env@Env{..} logfile pdir name version expected = do
    hfs <- liftIO $ readTVarIO envHaddocks
    prefix <- packageCmdPrefix name
    exitCode <-
        cabal
             env
             Normal
             prefix
             logfile
             pdir
             (["haddock"
              ,"--hyperlink-source"
              ,"--html"
              ,"--hoogle"
              ,"--html-location=../$pkg-$version/"] ++
              map
                  (\(pkgVer,hf) ->
                        concat
                            [ "--haddock-options=--read-interface="
                            , "../"
                            , pkgVer
                            , "/,"
                            , FP.encodeString hf])
                  (M.toList hfs))
    case (exitCode, expected) of
        (ExitSuccess,ExpectFailure) ->
            logLn env Normal (prefix <> "expected failure for haddock generation, but it succeeded!")
        (ExitFailure{},ExpectSuccess) ->
            do logLn env Normal (prefix <> "expected success for haddock, but it failed!")
               failed env exitCode
        _ -> return ()
    copy
    where
        ident = nameVer name version
        copy = do
            liftIO $
                do let orig = pkgDocDir env name version
                   exists <- FP.isDirectory orig
                   when exists $
                       renameOrCopy orig (pbDocDir envPB <> FP.decodeString ident)
            enewPath <- liftIO $ try $
                FP.canonicalizePath
                    (pbDocDir envPB <> FP.decodeString ident <>
                     FP.decodeString (display name ++ ".haddock"))
            case enewPath of
                Left (_ :: IOException) -> return () -- FIXME: log it with Shake.
                Right newPath -> liftIO $ atomically $ modifyTVar envHaddocks $
                    M.insert (ident) newPath

--------------------------------------------------------------------------------
-- Running commands

-- | Get a command prefix including progress.
packageCmdPrefix :: MonadIO m => PackageName -> m Text
packageCmdPrefix name =
    return (T.pack (display name) <> ": ")

-- | Run a command with the right envornment, logs the command being
-- run and its output as verbose mode.
cabal :: MonadIO m => Env -> Verbosity -> Text -> FilePath -> FilePath -> [String] -> m ExitCode
cabal env verbosity prefix logfile cwd args = do
    pwd <- liftIO FP.getWorkingDirectory
    envmap <- liftIO $ fmap (++ defaultEnv (envPB env) pwd) $ getEnvironment
    logLn env verbosity (prefix <> T.pack (fromMaybe "" (listToMaybe args)))
    logLn env Verbose (prefix <> T.pack (unwords (cmd' : map show args)))
    liftIO (FP.createTree (FP.directory logfile))
    code <- liftIO $ flip catch exitFailing
                   $ withBinaryFile (FP.encodeString logfile) AppendMode $ \outH ->
        do withCheckedProcess
               (proc cmd' args)
               { cwd = Just (FP.encodeString cwd)
               , std_err = UseHandle outH
               , std_out = UseHandle outH
               , env = Just envmap
               }
               (\ClosedStream UseProvidedHandle UseProvidedHandle ->
                     (return ()))
           return ExitSuccess
    case code of
      ExitFailure{} ->
          logLn env Normal
                    (prefix <> T.pack (fromMaybe "" (listToMaybe args)) <> ": " <>
                     "FAIL")
      ExitSuccess{} -> return ()
    return code
    where cmd' = "cabal" :: String
          exitFailing :: ProcessExitedUnsuccessfully -> IO ExitCode
          exitFailing (ProcessExitedUnsuccessfully _ code) = do
              FP.readFile logfile >>= logLn env Normal
              return code

-- | A result failed.
failed :: MonadIO m => Env -> ExitCode -> m ()
failed env code = liftIO
        (atomically
             (writeTVar (envStatus env) code))

-- | The action must return a success code or an exception is thrown.
succeed :: MonadIO m
        => m ExitCode -> m ()
succeed m = do
    v <- m
    case v of
        ExitFailure{} -> throw v
        ExitSuccess -> return ()

--------------------------------------------------------------------------------
-- Logging utilities

data Verbosity
    = Verbose
    | Normal

-- | Convenience.
class ToBS a where toBS :: a -> ByteString
instance ToBS String where toBS = toBS . T.pack
instance ToBS Text where toBS = T.encodeUtf8
instance ToBS ByteString where toBS = id

-- | Log to wherever is configured by the calling code.
logLn :: (MonadIO m,ToBS str) => Env -> Verbosity -> str -> m ()
logLn env v s = log env v (toBS s <> "\n")

-- | Log to wherever is configured by the calling code.
log :: (MonadIO m,ToBS str) => Env -> Verbosity -> str -> m ()
log env v s =
    when ((bool && verbose) || not bool)
         (liftIO
              (withMVar (envMsgLock env)
                        (const (pbLog
                                    (envPB env)
                                    (toBS s)))))
    where verbose = pbVerbose (envPB env)
          bool = case v of
                  Verbose -> True
                  Normal -> False
