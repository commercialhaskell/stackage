module Stackage.Test
    ( runTestSuites
    ) where

import           Control.Monad    (foldM, unless, when)
import qualified Data.Map         as Map
import qualified Data.Set         as Set
import           Stackage.Config
import           Stackage.Types
import           Stackage.Util
import           System.Directory (createDirectory, removeFile)
import           System.Exit      (ExitCode (ExitSuccess))
import           System.FilePath  ((<.>), (</>))
import           System.IO        (IOMode (WriteMode, AppendMode),
                                   withBinaryFile)
import           System.Process   (runProcess, waitForProcess)

runTestSuites :: InstallInfo -> IO ()
runTestSuites ii = do
    let testdir = "runtests"
    rm_r testdir
    createDirectory testdir
    allPass <- foldM (runTestSuite testdir) True $ Map.toList $ iiPackages ii
    unless allPass $ error $ "There were failures, please see the logs in " ++ testdir

runTestSuite :: FilePath -> Bool -> (PackageName, Version) -> IO Bool
runTestSuite testdir prevPassed pair@(packageName, _) = do
    passed <- do
        ph1 <- getHandle WriteMode $ \handle -> runProcess "cabal" ["unpack", package] (Just testdir) Nothing Nothing (Just handle) (Just handle)
        ec1 <- waitForProcess ph1
        if (ec1 /= ExitSuccess)
            then return False
            else do
                ph2 <- getHandle AppendMode $ \handle -> runProcess "cabal-dev" ["-s", "../../cabal-dev", "configure", "--enable-tests"] (Just dir) Nothing Nothing (Just handle) (Just handle)
                ec2 <- waitForProcess ph2
                if (ec2 /= ExitSuccess)
                    then return False
                    else do
                        ph3 <- getHandle AppendMode $ \handle -> runProcess "cabal-dev" ["build"] (Just dir) Nothing Nothing (Just handle) (Just handle)
                        ec3 <- waitForProcess ph3
                        if (ec3 /= ExitSuccess)
                            then return False
                            else do
                                ph4 <- getHandle AppendMode $ \handle -> runProcess "cabal-dev" ["test"] (Just dir) Nothing Nothing (Just handle) (Just handle)
                                ec4 <- waitForProcess ph4
                                if (ec4 /= ExitSuccess)
                                    then return False
                                    else do
                                        ph5 <- getHandle AppendMode $ \handle -> runProcess "cabal-dev" ["haddock"] (Just dir) Nothing Nothing (Just handle) (Just handle)
                                        ec5 <- waitForProcess ph5
                                        return $ ec5 == ExitSuccess
    let expectedFailure = packageName `Set.member` expectedFailures
    if passed
        then do
            removeFile logfile
            when expectedFailure $ putStrLn $ package ++ " passed, but I didn't think it would."
        else unless expectedFailure $ putStrLn $ "Test suite failed: " ++ package
    rm_r dir
    return $! prevPassed && (passed || expectedFailure)
  where
    logfile = testdir </> package <.> "log"
    dir = testdir </> package
    getHandle mode = withBinaryFile logfile mode
    package = packageVersionString pair
