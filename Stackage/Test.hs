{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}
module Stackage.Test
    ( runTestSuites
    ) where

import qualified Control.Concurrent as C
import           Control.Exception  (Exception, SomeException, handle, throwIO)
import           Control.Monad      (replicateM, unless, when)
import qualified Data.Map           as Map
import qualified Data.Set           as Set
import           Data.Typeable      (Typeable)
import           Stackage.Types
import           Stackage.Util
import           System.Directory   (createDirectory, removeFile)
import           System.Exit        (ExitCode (ExitSuccess))
import           System.FilePath    ((<.>), (</>))
import           System.IO          (IOMode (WriteMode, AppendMode),
                                     withBinaryFile)
import           System.Process     (runProcess, waitForProcess)

runTestSuites :: BuildSettings -> BuildPlan -> IO ()
runTestSuites settings' bp = do
    settings <- fixBuildSettings settings'
    let selected = Map.filterWithKey notSkipped $ bpPackages bp
    putStrLn "Running test suites"
    let testdir = "runtests"
    rm_r testdir
    createDirectory testdir
    allPass <- parFoldM (testWorkerThreads settings) (runTestSuite settings testdir) (&&) True $ Map.toList selected
    unless allPass $ error $ "There were failures, please see the logs in " ++ testdir
  where
    notSkipped p _ = p `Set.notMember` bpSkippedTests bp

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

runTestSuite :: BuildSettings
             -> FilePath
             -> (PackageName, SelectedPackageInfo)
             -> IO Bool
runTestSuite settings testdir (packageName, SelectedPackageInfo {..}) = do
    -- Set up a new environment that includes the sandboxed bin folder in PATH.
    env' <- getModifiedEnv settings
    let menv addGPP
            = Just $ (if addGPP then (("GHC_PACKAGE_PATH", packageDir settings ++ ":"):) else id)
                   $ ("HASKELL_PACKAGE_SANDBOX", packageDir settings)
                   : env'

    let runGen addGPP cmd args wdir handle' = do
            ph <- runProcess cmd args (Just wdir) (menv addGPP) Nothing (Just handle') (Just handle')
            ec <- waitForProcess ph
            unless (ec == ExitSuccess) $ throwIO TestException

    let run = runGen False
        runGhcPackagePath = runGen True

    passed <- handle (\TestException -> return False) $ do
        package' <- replaceTarball (tarballDir settings) package
        getHandle WriteMode  $ run "cabal" ["unpack", package'] testdir
        getHandle AppendMode $ run "cabal" (addCabalArgs settings BSTest ["configure", "--enable-tests"]) dir
        when spiHasTests $ do
            getHandle AppendMode $ run "cabal" ["build"] dir
            getHandle AppendMode $ runGhcPackagePath "cabal" ["test"] dir
        when (buildDocs settings) $
            getHandle AppendMode $ run "cabal" ["haddock"] dir
        return True
    let expectedFailure = packageName `Set.member` expectedFailuresBuild settings
    if passed
        then do
            removeFile logfile
            when expectedFailure $ putStrLn $ package ++ " passed, but I didn't think it would."
        else unless expectedFailure $ putStrLn $ concat
                [ "Test suite failed: "
                , package
                , "("
                , unMaintainer spiMaintainer
                , case spiGithubUser of
                    Nothing -> ""
                    Just x -> " @" ++ x
                , ")"
                ]
    rm_r dir
    return $! passed || expectedFailure
  where
    logfile = testdir </> package <.> "log"
    dir = testdir </> package
    getHandle mode = withBinaryFile logfile mode
    package = packageVersionString (packageName, spiVersion)
