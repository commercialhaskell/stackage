{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Stackage.Test
    ( runTestSuites
    ) where

import           Control.Monad      (foldM, unless, when)
import qualified Data.Map           as Map
import qualified Data.Set           as Set
import           Stackage.Config
import           Stackage.Types
import           Stackage.Util
import           System.Directory   (createDirectory, removeFile, canonicalizePath)
import           System.Environment (getEnvironment)
import           System.Exit        (ExitCode (ExitSuccess))
import           System.FilePath    ((<.>), (</>))
import           System.IO          (IOMode (WriteMode, AppendMode),
                                     withBinaryFile)
import           System.Process     (runProcess, waitForProcess)
import Control.Exception (handle, Exception, throwIO)
import Data.Typeable (Typeable)

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

-- | Separate for the PATH environment variable
pathSep :: Char
#ifdef mingw32_HOST_OS
pathSep = ';'
#else
pathSep = ':'
#endif

fixEnv :: FilePath -> (String, String) -> (String, String)
fixEnv bin (p@"PATH", x) = (p, bin ++ pathSep : x)
fixEnv _ x = x

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
    env' <- getEnvironment
    let menv = Just $ map (fixEnv $ binDir settings) env'

    let run cmd args wdir handle = do
            ph <- runProcess cmd args (Just wdir) menv Nothing (Just handle) (Just handle)
            ec <- waitForProcess ph
            unless (ec == ExitSuccess) $ throwIO TestException

    passed <- handle (\TestException -> return False) $ do
        getHandle WriteMode  $ run "cabal" ["unpack", package] testdir
        getHandle AppendMode $ run "cabal" (addCabalArgs settings ["configure", "--enable-tests"]) dir
        when (hasTestSuites packageName) $ do
            getHandle AppendMode $ run "cabal" ["build"] dir
            getHandle AppendMode $ run "cabal" ["test"] dir
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
