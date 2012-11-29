module Stackage.Build
    ( build
    , defaultBuildSettings
    , BuildSettings (..)
    ) where

import           Distribution.Text       (simpleParse)
import           Control.Monad        (unless)
import           Stackage.Types
import           Stackage.CheckPlan
import           Stackage.InstallInfo
import           Stackage.Tarballs
import           Stackage.Test
import           Stackage.Util
import           Stackage.Config
import           System.Exit          (ExitCode (ExitSuccess), exitWith)
import           System.IO            (IOMode (WriteMode), withBinaryFile)
import           System.Process       (runProcess, waitForProcess, rawSystem, readProcess)
import           System.Directory     (createDirectoryIfMissing, canonicalizePath)
import           Distribution.Version    (thisVersion, withinRange)
import Control.Exception (assert)

defaultBuildSettings :: BuildSettings
defaultBuildSettings = BuildSettings
    { sandboxRoot = "sandbox"
    , extraBuildArgs = []
    , extraCore = defaultExtraCore
    , expectedFailures = defaultExpectedFailures
    , stablePackages = defaultStablePackages
    , extraArgs = ["-fnetwork23"]
    }

build :: BuildSettings -> IO ()
build settings' = do
    putStrLn "Creating a build plan"
    ii <- getInstallInfo settings'

    putStrLn "Wiping out old sandbox folder"
    let root' = sandboxRoot settings'
    rm_r root'
    rm_r "logs"
    createDirectoryIfMissing True root'
    root <- canonicalizePath root'
    let settings = settings' { sandboxRoot = root }

    ec1 <- rawSystem "ghc-pkg" ["init", packageDir settings]
    unless (ec1 == ExitSuccess) $ do
        putStrLn "Unable to create package database via ghc-pkg init"
        exitWith ec1

    checkPlan settings ii
    putStrLn "No mismatches, starting the sandboxed build."

    versionString <- readProcess "cabal" ["--version"] ""
    libVersion <-
        case map words $ lines versionString of
            [_,["using","version",libVersion,"of","the","Cabal","library"]] -> return libVersion
            _ -> error "Did not understand cabal --version output"

    case (simpleParse libVersion, simpleParse ">= 1.16") of
        (Nothing, _) -> error $ "Invalid Cabal library version: " ++ libVersion
        (_, Nothing) -> assert False $ return ()
        (Just v, Just vr)
            | v `withinRange` vr -> return ()
            | otherwise -> error $ "Unsupported Cabal version: " ++ libVersion

    ph <- withBinaryFile "build.log" WriteMode $ \handle ->
        let args = addCabalArgs settings
                 $ "install"
                 : ("--cabal-lib-version=" ++ libVersion)
                 : "--build-log=logs/$pkg.log"
                 : "-j"
                 : concat
                    [ extraBuildArgs settings
                    , extraArgs settings
                    , iiPackageList ii
                    ]
         in runProcess "cabal" args Nothing Nothing Nothing (Just handle) (Just handle)
    ec <- waitForProcess ph
    unless (ec == ExitSuccess) $ do
        putStrLn "Build failed, please see build.log"
        exitWith ec

    putStrLn "Sandbox built, beginning individual test suites"
    runTestSuites settings ii

    putStrLn "All test suites that were expected to pass did pass, building tarballs."
    makeTarballs ii
