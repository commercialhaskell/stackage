-- | Perform an actual build, generate a binary package database and a
-- documentation directory in the process.
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
module Stackage.PerformBuild
    ( performBuild
    , PerformBuild (..)
    , BuildException (..)
    , pbDocDir
    ) where

import Stackage.BuildConstraints
import Stackage.PackageDescription
import Stackage.BuildPlan
import Stackage.Prelude hiding (pi)
import qualified Data.Map as Map
import Control.Concurrent.STM.TSem
import Data.NonNull (fromNullable)
import Control.Concurrent.Async (async)
import System.IO.Temp (withSystemTempDirectory)
import Filesystem (createTree, removeTree, isDirectory, rename, canonicalizePath, getWorkingDirectory)
import System.IO (withBinaryFile, IOMode (WriteMode))
import Filesystem.Path (parent)
import qualified Filesystem.Path as F
import System.Environment (getEnvironment)
import System.Directory (findExecutable)

data BuildException = BuildException (Map PackageName BuildFailure) [Text]
    deriving Typeable
instance Exception BuildException
instance Show BuildException where
    show (BuildException m warnings) =
        unlines $ map go (mapToList m) ++ map unpack warnings
      where
        go (PackageName name, bf) = concat
            [ name
            , ": "
            , show bf
            ]

data BuildFailure = DependencyFailed PackageName
                  | DependencyMissing PackageName
                  | ToolMissing ExeName
                  | NotImplemented
                  | BuildFailureException SomeException
    deriving (Show, Typeable)
instance Exception BuildFailure

data PerformBuild = PerformBuild
    { pbPlan :: BuildPlan
    , pbInstallDest :: FilePath
    , pbLog :: ByteString -> IO ()
    , pbLogDir :: FilePath
    , pbJobs :: Int
    }

data PackageInfo = PackageInfo
    { piPlan :: PackagePlan
    , piName :: PackageName
    , piResult :: TMVar Bool
    }

waitForDeps :: Map ExeName (Set PackageName)
            -> Map PackageName PackageInfo
            -> Set Component
            -> BuildPlan
            -> PackageInfo
            -> IO a
            -> IO a
waitForDeps toolMap packageMap activeComps bp pi action = do
    atomically $ do
        mapM_ checkPackage $ Map.keys $ filterUnused $ sdPackages $ ppDesc $ piPlan pi
        forM_ (Map.keys $ filterUnused $ sdTools $ ppDesc $ piPlan pi) $ \exe -> do
            case lookup exe toolMap >>= fromNullable . map checkPackage . setToList of
                Nothing
                    | isCoreExe exe -> return ()
                    | otherwise -> throwSTM $ ToolMissing exe
                Just packages -> ofoldl1' (<|>) packages
    action
  where
    filterUnused :: Ord key => Map key DepInfo -> Map key DepInfo
    filterUnused =
        mapFromList . filter (go . snd) . mapToList
      where
        go = not . null . intersection activeComps . diComponents

    checkPackage package | package == piName pi = return ()
    checkPackage package =
        case lookup package packageMap of
            Nothing
                | isCore package -> return ()
                | otherwise -> throwSTM $ DependencyMissing package
            Just dep -> do
                res <- readTMVar $ piResult dep
                unless res $ throwSTM $ DependencyFailed package

    isCore = (`member` siCorePackages (bpSystemInfo bp))
    isCoreExe = (`member` siCoreExecutables (bpSystemInfo bp))

withCounter counter = bracket_
    (atomically $ modifyTVar counter (+ 1))
    (atomically $ modifyTVar counter (subtract 1))

withTSem sem = bracket_ (atomically $ waitTSem sem) (atomically $ signalTSem sem)

pbDatabase pb = pbInstallDest pb </> "pkgdb"
pbBinDir pb = pbInstallDest pb </> "bin"
pbLibDir pb = pbInstallDest pb </> "lib"
pbDataDir pb = pbInstallDest pb </> "share"
pbDocDir pb = pbInstallDest pb </> "doc"

performBuild :: PerformBuild -> IO [Text]
performBuild pb = do
    cwd <- getWorkingDirectory
    performBuild' pb
        { pbInstallDest = cwd </> pbInstallDest pb
        , pbLogDir = cwd </> pbLogDir pb
        }

performBuild' :: PerformBuild -> IO [Text]
performBuild' pb@PerformBuild {..} = withBuildDir $ \builddir -> do
    let removeTree' fp = whenM (isDirectory fp) (removeTree fp)
    mapM_ removeTree' [pbInstallDest, pbLogDir]

    createTree $ parent $ pbDatabase pb
    withCheckedProcess (proc "ghc-pkg" ["init", fpToString (pbDatabase pb)])
        $ \ClosedStream Inherited Inherited -> return ()
    pbLog $ encodeUtf8 "Copying built-in Haddocks\n"
    copyBuiltInHaddocks (pbDocDir pb)

    sem <- atomically $ newTSem pbJobs
    active <- newTVarIO (0 :: Int)
    let toolMap = makeToolMap $ bpPackages pbPlan
    packageMap <- fmap fold $ forM (mapToList $ bpPackages pbPlan)
        $ \(name, plan) -> do
            let piPlan = plan
                piName = name
            piResult <- newEmptyTMVarIO
            return $ singletonMap name PackageInfo {..}

    errsVar <- newTVarIO mempty
    warningsVar <- newTVarIO id
    mutex <- newMVar ()
    env <- getEnvironment
    haddockFiles <- newTVarIO mempty

    forM_ packageMap $ \pi -> void $ async $ singleBuild pb SingleBuild
        { sbSem = sem
        , sbErrsVar = errsVar
        , sbWarningsVar = warningsVar
        , sbActive = active
        , sbToolMap = toolMap
        , sbPackageMap = packageMap
        , sbBuildDir = builddir
        , sbPackageInfo = pi
        , sbRegisterMutex = mutex
        , sbModifiedEnv = ("HASKELL_PACKAGE_SANDBOX", fpToString $ pbDatabase pb)
                        : map fixEnv env
        , sbHaddockFiles = haddockFiles
        }

    void $ tryAny $ atomically $ readTVar active >>= checkSTM . (== 0)

    warnings <- ($ []) <$> readTVarIO warningsVar
    errs <- readTVarIO errsVar
    when (not $ null errs) $ throwM $ BuildException errs warnings
    return warnings
  where
    withBuildDir f = withSystemTempDirectory "stackage-build" (f . fpFromString)

    fixEnv (p, x)
        -- Thank you Windows having case-insensitive environment variables...
        | toUpper p == "PATH" = (p, fpToString (pbBinDir pb) ++ pathSep : x)
        | otherwise = (p, x)

    -- | Separate for the PATH environment variable
    pathSep :: Char
#ifdef mingw32_HOST_OS
    pathSep = ';'
#else
    pathSep = ':'
#endif

data SingleBuild = SingleBuild
    { sbSem :: TSem
    , sbErrsVar :: TVar (Map PackageName BuildFailure)
    , sbWarningsVar :: TVar ([Text] -> [Text])
    , sbActive :: TVar Int
    , sbToolMap :: Map ExeName (Set PackageName)
    , sbPackageMap :: Map PackageName PackageInfo
    , sbBuildDir :: FilePath
    , sbPackageInfo :: PackageInfo
    , sbRegisterMutex :: MVar ()
    , sbModifiedEnv :: [(String, String)]
    , sbHaddockFiles :: TVar (Map Text FilePath) -- ^ package-version, .haddock file
    }

singleBuild :: PerformBuild -> SingleBuild -> IO ()
singleBuild pb@PerformBuild {..} SingleBuild {..} =
      withCounter sbActive
    $ handle updateErrs
    $ (`finally` void (atomically $ tryPutTMVar (piResult sbPackageInfo) False))
    $ inner
  where
    libComps = setFromList [CompLibrary, CompExecutable]
    testComps = insertSet CompTestSuite libComps
    inner = do
        let wfd comps =
                waitForDeps sbToolMap sbPackageMap comps pbPlan sbPackageInfo
                . withTSem sbSem
        wfd libComps buildLibrary

        wfd testComps runTests

    name = display $ piName sbPackageInfo
    namever = concat
        [ name
        , "-"
        , display $ ppVersion $ piPlan sbPackageInfo
        ]

    runIn wdir outH cmd args =
        withCheckedProcess cp $ \ClosedStream UseProvidedHandle UseProvidedHandle ->
            (return () :: IO ())
      where
        cp = (proc (unpack $ asText cmd) (map (unpack . asText) args))
            { cwd = Just $ fpToString wdir
            , std_out = UseHandle outH
            , std_err = UseHandle outH
            , env = Just sbModifiedEnv
            }
    runParent = runIn sbBuildDir
    runChild = runIn childDir
    childDir = sbBuildDir </> fpFromText namever

    log' t = do
        i <- readTVarIO sbActive
        errs <- readTVarIO sbErrsVar
        pbLog $ encodeUtf8 $ concat
            [ t
            , " (pending: "
            , tshow i
            , ", failures: "
            , tshow $ length errs
            , ")\n"
            ]
    libOut = pbLogDir </> fpFromText namever </> "build.out"
    testOut = pbLogDir </> fpFromText namever </> "test.out"
    testRunOut = pbLogDir </> fpFromText namever </> "test-run.out"

    wf fp inner = do
        createTree $ parent fp
        withBinaryFile (fpToString fp) WriteMode inner

    configArgs =
        [ "--package-db=clear"
        , "--package-db=global"
        , "--package-db=" ++ fpToText (pbDatabase pb)
        , "--libdir=" ++ fpToText (pbLibDir pb)
        , "--bindir=" ++ fpToText (pbBinDir pb)
        , "--datadir=" ++ fpToText (pbDataDir pb)
        , "--docdir=" ++ fpToText (pbDocDir pb)
        , "--flags=" ++ flags
        ]

    flags :: Text
    flags = unwords $ map go $ mapToList pcFlagOverrides
      where
        go (name, isOn) = concat
            [ if isOn then "" else "-"
            , unFlagName name
            ]

    PackageConstraints {..} = ppConstraints $ piPlan sbPackageInfo

    buildLibrary = wf libOut $ \outH -> do
        let run = runChild outH
        log' $ "Unpacking " ++ namever
        runParent outH "cabal" ["unpack", namever]

        log' $ "Configuring " ++ namever
        run "cabal" $ "configure" : configArgs

        log' $ "Building " ++ namever
        run "cabal" ["build"]

        log' $ "Copying/registering " ++ namever
        run "cabal" ["copy"]
        withMVar sbRegisterMutex $ const $
            run "cabal" ["register"]

        -- Even if the tests later fail, we can allow other libraries to build
        -- on top of our successful results
        --
        -- FIXME do we need to wait to do this until after Haddocks build?
        -- otherwise, we could have a race condition and try to build a
        -- dependency's haddocks before this finishes
        atomically $ putTMVar (piResult sbPackageInfo) True

        when (pcHaddocks /= Don'tBuild && not (null $ sdModules $ ppDesc $ piPlan sbPackageInfo)) $ do
            log' $ "Haddocks " ++ namever
            hfs <- readTVarIO sbHaddockFiles
            let hfsOpts = flip map (mapToList hfs) $ \(pkgVer, hf) -> concat
                    [ "--haddock-options=--read-interface="
                    , "../"
                    , pkgVer
                    , "/,"
                    , fpToText hf
                    ]
                args = "haddock"
                     : "--hyperlink-source"
                     : "--html"
                     : "--hoogle"
                     : "--html-location=../$pkg-$version/"
                     : hfsOpts

            eres <- tryAny $ run "cabal" args

            forM_ eres $ \() -> do
                renameOrCopy
                    (childDir </> "dist" </> "doc" </> "html" </> fpFromText name)
                    (pbDocDir pb </> fpFromText namever)

                enewPath <- tryIO
                          $ canonicalizePath
                          $ pbDocDir pb
                        </> fpFromText namever
                        </> fpFromText name <.> "haddock"
                case enewPath of
                    Left e -> warn $ tshow e
                    Right newPath -> atomically
                                   $ modifyTVar sbHaddockFiles
                                   $ insertMap namever newPath

            case (eres, pcHaddocks) of
                (Left e, ExpectSuccess) -> throwM e
                (Right (), ExpectFailure) -> warn $ namever ++ ": unexpected Haddock success"
                _ -> return ()

    runTests = wf testOut $ \outH -> do
        let run = runChild outH

        when (pcTests /= Don'tBuild) $ do
            log' $ "Test configure " ++ namever
            run "cabal" $ "configure" : "--enable-tests" : configArgs

            eres <- tryAny $ do
                log' $ "Test build " ++ namever
                run "cabal" ["build"]

                log' $ "Test run " ++ namever
                run "cabal" ["test", "--log=" ++ fpToText testRunOut]

            case (eres, pcTests) of
                (Left e, ExpectSuccess) -> throwM e
                (Right (), ExpectFailure) -> warn $ namever ++ ": unexpected test success"
                _ -> return ()

    warn t = atomically $ modifyTVar sbWarningsVar (. (t:))

    updateErrs exc = do
        log' $ concat
            [ display (piName sbPackageInfo)
            , ": "
            , tshow exc
            ]
        atomically $ modifyTVar sbErrsVar $ insertMap (piName sbPackageInfo) exc'
      where
        exc' =
            case fromException exc of
                Just bf -> bf
                Nothing -> BuildFailureException exc

renameOrCopy :: FilePath -> FilePath -> IO ()
renameOrCopy src dest = rename src dest `catchIO` \_ -> copyDir src dest

copyDir :: FilePath -> FilePath -> IO ()
copyDir src dest =
    runResourceT $ sourceDirectoryDeep False src $$ mapM_C go
  where
    src' = src </> ""
    go fp = forM_ (F.stripPrefix src' fp) $ \suffix -> do
        let dest' = dest </> suffix
        liftIO $ createTree $ parent dest'
        sourceFile fp $$ (sinkFile dest' :: Sink ByteString (ResourceT IO) ())

copyBuiltInHaddocks :: FilePath -> IO ()
copyBuiltInHaddocks docdir = do
    mghc <- findExecutable "ghc"
    case mghc of
        Nothing -> error "GHC not found on PATH"
        Just ghc -> do
            src <- canonicalizePath
                (parent (fpFromString ghc) </> "../share/doc/ghc/html/libraries")
            copyDir src docdir
