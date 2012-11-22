module Stackage.Build
    ( build
    ) where

import           Control.Monad        (unless)
import           Stackage.CheckPlan
import           Stackage.InstallInfo
import           Stackage.Tarballs
import           Stackage.Test
import           Stackage.Util
import           System.Exit          (ExitCode (ExitSuccess), exitWith)
import           System.IO            (IOMode (WriteMode), withBinaryFile)
import           System.Process       (runProcess, waitForProcess)

build :: IO ()
build = do
    ii <- getInstallInfo

    rm_r "cabal-dev"

    checkPlan ii
    putStrLn "No mismatches, starting the sandboxed build."

    ph <- withBinaryFile "build.log" WriteMode $ \handle ->
        runProcess "cabal-dev" ("install":"-fnetwork23":iiPackageList ii) Nothing Nothing Nothing (Just handle) (Just handle)
    ec <- waitForProcess ph
    unless (ec == ExitSuccess) $ exitWith ec

    putStrLn "Sandbox built, beginning individual test suites"
    runTestSuites ii

    putStrLn "All test suites that were expected to pass did pass, building tarballs."
    makeTarballs ii
