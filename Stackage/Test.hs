{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}
module Stackage.Test
    ( runTestSuites
    ) where

import qualified Control.Concurrent as C
import           Control.Exception  (Exception, SomeException, handle, throwIO)
import           Control.Monad      (replicateM, unless, when, forM_)
import qualified Data.Map           as Map
import qualified Data.Set           as Set
import           Data.Version       (parseVersion, Version (Version))
import           Data.Typeable      (Typeable)
import           Stackage.Types
import           Stackage.Util
import           System.Directory   (copyFile, createDirectory,
                                     createDirectoryIfMissing, doesFileExist, findExecutable,
                                     getDirectoryContents, removeFile,
                                     renameDirectory)
import           System.Exit        (ExitCode (ExitSuccess))
import           System.FilePath    ((<.>), (</>), takeDirectory)
import           System.IO          (IOMode (WriteMode, AppendMode),
                                     withBinaryFile)
import           System.Process     (readProcess, runProcess, waitForProcess)
import           Text.ParserCombinators.ReadP (readP_to_S)

runTestSuites :: BuildSettings -> BuildPlan -> IO ()
runTestSuites settings' bp = do
    settings <- fixBuildSettings settings'
    let selected = Map.filterWithKey notSkipped $ bpPackages bp
    putStrLn "Running test suites"
    let testdir = "runtests"
        docdir = "haddock"
    rm_r testdir
    rm_r docdir
    createDirectory testdir
    createDirectory docdir

    copyBuiltInHaddocks docdir

    cabalVersion <- getCabalVersion
    allPass <- parFoldM (testWorkerThreads settings) (runTestSuite cabalVersion settings testdir docdir) (&&) True $ Map.toList selected
    unless allPass $ error $ "There were failures, please see the logs in " ++ testdir
  where
    notSkipped p _ = p `Set.notMember` bpSkippedTests bp

getCabalVersion :: IO CabalVersion
getCabalVersion = do
    output <- readProcess "cabal" ["--numeric-version"] ""
    case filter (null . snd) $ readP_to_S parseVersion $ filter notCRLF output of
        (Version (x:y:_) _, _):_ -> return $ CabalVersion x y
        _ -> error $ "Invalid cabal version: " ++ show output
  where
    notCRLF '\n' = False
    notCRLF '\r' = False
    notCRLF _    = True

parFoldM :: Int -- ^ number of threads
         -> (b -> IO c)
         -> (a -> c -> a)
         -> a
         -> [b]
         -> IO a
parFoldM threadCount0 f g a0 bs0 = do
    ma <- C.newMVar a0
    mbs <- C.newMVar bs0
    signal <- C.newEmptyMVar
    tids <- replicateM threadCount0 $ C.forkIO $ worker ma mbs signal
    wait threadCount0 signal tids
    C.takeMVar ma
  where
    worker ma mbs signal =
        handle
            (C.putMVar signal . Just)
            (loop >> C.putMVar signal Nothing)
      where
        loop = do
            mb <- C.modifyMVar mbs $ \bs -> return $
                case bs of
                    [] -> ([], Nothing)
                    b:bs' -> (bs', Just b)
            case mb of
                Nothing -> return ()
                Just b -> do
                    c <- f b
                    C.modifyMVar_ ma $ \a -> return $! g a c
                    loop
    wait threadCount signal tids
        | threadCount == 0 = return ()
        | otherwise = do
            me <- C.takeMVar signal
            case me of
                Nothing -> wait (threadCount - 1) signal tids
                Just e -> do
                    mapM_ C.killThread tids
                    throwIO (e :: SomeException)

data TestException = TestException
    deriving (Show, Typeable)
instance Exception TestException

data CabalVersion = CabalVersion Int Int
    deriving (Eq, Ord, Show)

runTestSuite :: CabalVersion
             -> BuildSettings
             -> FilePath -- ^ testdir
             -> FilePath -- ^ docdir
             -> (PackageName, SelectedPackageInfo)
             -> IO Bool
runTestSuite cabalVersion settings testdir docdir (packageName, SelectedPackageInfo {..}) = do
    -- Set up a new environment that includes the sandboxed bin folder in PATH.
    env' <- getModifiedEnv settings
    let menv = Just $ addSandbox env'
        addSandbox = (("HASKELL_PACKAGE_SANDBOX", packageDir settings):)

    let run cmd args wdir handle' = do
            ph <- runProcess cmd args (Just wdir) menv Nothing (Just handle') (Just handle')
            ec <- waitForProcess ph
            unless (ec == ExitSuccess) $ throwIO TestException

    passed <- handle (\TestException -> return False) $ do
        package' <- replaceTarball (tarballDir settings) package
        getHandle WriteMode  $ run "cabal" ["unpack", package'] testdir
        case cabalFileDir settings of
            Nothing -> return ()
            Just cfd -> do
                let PackageName name = packageName
                    basename = name ++ ".cabal"
                    src = dir </> basename
                    dst = cfd </> basename
                createDirectoryIfMissing True cfd
                copyFile src dst
        getHandle AppendMode $ run "cabal" (addCabalArgs settings BSTest ["configure", "--enable-tests"]) dir
        when spiHasTests $ do
            getHandle AppendMode $ run "cabal" ["build"] dir
            getHandle AppendMode $ run "cabal" (concat
                [ ["test"]
                , if cabalVersion >= CabalVersion 1 20
                    then ["--show-details=streaming"] -- FIXME temporary workaround for https://github.com/haskell/cabal/issues/1810
                    else []
                ]) dir
        when (buildDocs settings) $ do
            getHandle AppendMode $ run "cabal"
                [ "haddock"
                , "--hyperlink-source"
                , "--html"
                , "--hoogle"
                , "--html-location=../$pkg-$version/"
                ] dir
            let PackageName packageName' = packageName
            renameDirectory
                (dir </> "dist" </> "doc" </> "html" </> packageName')
                (docdir </> package)
        return True
    let expectedFailure = packageName `Set.member` expectedFailuresBuild settings
    if passed
        then do
            removeFile logfile
            when expectedFailure $ putStrLn $ "   " ++ package ++ " passed, but I didn't think it would."
        else unless expectedFailure $ putStrLn $ concat
                [ "Test suite failed: "
                , package
                , "("
                , unMaintainer spiMaintainer
                , githubMentions spiGithubUser
                , ")"
                ]
    rm_r dir
    return $! passed || expectedFailure
  where
    logfile = testdir </> package <.> "log"
    dir = testdir </> package
    getHandle mode = withBinaryFile logfile mode
    package = packageVersionString (packageName, spiVersion)

copyBuiltInHaddocks docdir = do
    Just ghc <- findExecutable "ghc"
    copyTree (takeDirectory ghc </> "../share/doc/ghc/html/libraries") docdir
  where
    copyTree src dest = do
        entries <- fmap (filter (\s -> s /= "." && s /= ".."))
                 $ getDirectoryContents src
        forM_ entries $ \entry -> do
            let src' = src </> entry
                dest' = dest </> entry
            isFile <- doesFileExist src'
            if isFile
                then copyFile src' dest'
                else do
                    createDirectory dest'
                    copyTree src' dest'
