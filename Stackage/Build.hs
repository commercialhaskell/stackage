module Stackage.Build
    ( build
    ) where

import           Control.Monad            (unless)
import           Stackage.CheckPlan
import           Stackage.Test
import           Stackage.Util
import           Stackage.InstallInfo
import           System.Process           (waitForProcess, runProcess)
import System.Exit (ExitCode (ExitSuccess), exitWith)
import System.IO (IOMode (WriteMode), withBinaryFile)

build :: IO ()
build = do
    ii <- getInstallInfo
    checkPlan ii

    rm_r "cabal-dev"

    putStrLn "No mismatches, good to go!"

    ph <- withBinaryFile "build.log" WriteMode $ \handle ->
        runProcess "cabal-dev" ("install":"-fnetwork23":iiPackageList ii) Nothing Nothing Nothing (Just handle) (Just handle)
    ec <- waitForProcess ph
    unless (ec == ExitSuccess) $ exitWith ec

    putStrLn "Environment built, beginning individual test suites"

    runTestSuites ii
