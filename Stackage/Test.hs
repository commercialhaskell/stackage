{-# LANGUAGE DeriveDataTypeable #-}
module Stackage.Test
    ( runTestSuites
    ) where

import           Control.Exception  (Exception, handle, throwIO)
import           Control.Monad      (foldM, unless, when)
import qualified Data.Map           as Map
import qualified Data.Set           as Set
import           Data.Typeable      (Typeable)
import           Stackage.Config
import           Stackage.Types
import           Stackage.Util
import           System.Directory   (canonicalizePath, createDirectory,
                                     removeFile)
import           System.Environment (getEnvironment)
import           System.Exit        (ExitCode (ExitSuccess))
import           System.FilePath    ((<.>), (</>))
import           System.IO          (IOMode (WriteMode, AppendMode),
                                     withBinaryFile)
import           System.Process     (runProcess, waitForProcess)

runTestSuites :: BuildSettings -> InstallInfo -> IO ()
runTestSuites settings ii = do
    let testdir = "runtests"
    rm_r testdir
    createDirectory testdir
    allPass <- foldM (runTestSuite settings testdir hasTestSuites) True $ Map.toList $ iiPackages ii
    unless allPass $ error $ "There were failures, please see the logs in " ++ testdir
  where
    PackageDB pdb = iiPackageDB ii

    hasTestSuites name = maybe defaultHasTestSuites piHasTests $ Map.lookup name pdb

data TestException = TestException
    deriving (Show, Typeable)
instance Exception TestException

runTestSuite :: BuildSettings
             -> FilePath
             -> (PackageName -> Bool) -- ^ do we have any test suites?
             -> Bool
             -> (PackageName, (Version, Maintainer))
             -> IO Bool
runTestSuite settings testdir hasTestSuites prevPassed (packageName, (version, Maintainer maintainer)) = do
    -- Set up a new environment that includes the sandboxed bin folder in PATH.
    env' <- getModifiedEnv settings
    let menv addGPP
            = Just $ (if addGPP then (("GHC_PACKAGE_PATH", packageDir settings ++ ":"):) else id)
                   $ ("HASKELL_PACKAGE_SANDBOX", packageDir settings)
                   : env'

    let runGen addGPP cmd args wdir handle = do
            ph <- runProcess cmd args (Just wdir) (menv addGPP) Nothing (Just handle) (Just handle)
            ec <- waitForProcess ph
            unless (ec == ExitSuccess) $ throwIO TestException

    let run = runGen False
        runGhcPackagePath = runGen True

    passed <- handle (\TestException -> return False) $ do
        getHandle WriteMode  $ run "cabal" ["unpack", package] testdir
        getHandle AppendMode $ run "cabal" (addCabalArgs settings ["configure", "--enable-tests"]) dir
        when (hasTestSuites packageName) $ do
            getHandle AppendMode $ run "cabal" ["build"] dir
            getHandle AppendMode $ runGhcPackagePath "cabal" ["test"] dir
        getHandle AppendMode $ run "cabal" ["haddock"] dir
        return True
    let expectedFailure = packageName `Set.member` expectedFailures settings
    if passed
        then do
            removeFile logfile
            when expectedFailure $ putStrLn $ package ++ " passed, but I didn't think it would."
        else unless expectedFailure $ putStrLn $ "Test suite failed: " ++ package ++ "(" ++ maintainer ++ ")"
    rm_r dir
    return $! prevPassed && (passed || expectedFailure)
  where
    logfile = testdir </> package <.> "log"
    dir = testdir </> package
    getHandle mode = withBinaryFile logfile mode
    package = packageVersionString (packageName, version)
