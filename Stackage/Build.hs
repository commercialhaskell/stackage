module Stackage.Build
    ( build
    , defaultBuildSettings
    , BuildSettings (..)
    ) where

import           Control.Monad        (unless)
import           Prelude              hiding (pi)
import           Stackage.Config
import           Stackage.InstallInfo
import           Stackage.Types
import           Stackage.Util
import           System.Exit          (ExitCode (ExitSuccess), exitWith)
import           System.IO            (IOMode (WriteMode), hPutStrLn,
                                       withBinaryFile)
import           System.Process       (rawSystem, runProcess,
                                       waitForProcess)
import Stackage.CheckCabalVersion (checkCabalVersion)

defaultBuildSettings :: BuildSettings
defaultBuildSettings = BuildSettings
    { sandboxRoot = "sandbox"
    , expectedFailuresBuild = defaultExpectedFailures
    , extraArgs = ["-fnetwork23"]
    , testWorkerThreads = 4
    }

build :: BuildSettings -> BuildPlan -> IO ()
build settings' bp = do
    libVersion <- checkCabalVersion

    putStrLn "Wiping out old sandbox folder"
    rm_r $ sandboxRoot settings'
    rm_r "logs"
    settings <- fixBuildSettings settings'

    putStrLn "Creating new package database"
    ec1 <- rawSystem "ghc-pkg" ["init", packageDir settings]
    unless (ec1 == ExitSuccess) $ do
        putStrLn "Unable to create package database via ghc-pkg init"
        exitWith ec1

    menv <- fmap Just $ getModifiedEnv settings
    let runCabal args handle = runProcess "cabal" args Nothing menv Nothing (Just handle) (Just handle)

    -- First install build tools so they can be used below.
    case bpTools bp of
        [] -> putStrLn "No build tools required"
        tools -> do
            putStrLn $ "Installing the following build tools: " ++ unwords tools
            ph1 <- withBinaryFile "build-tools.log" WriteMode $ \handle -> do
                let args = addCabalArgs settings
                         $ "install"
                         : ("--cabal-lib-version=" ++ libVersion)
                         : "--build-log=logs-tools/$pkg.log"
                         : "-j"
                         : concat
                            [ extraArgs settings
                            , tools
                            ]
                hPutStrLn handle ("cabal " ++ unwords (map (\s -> "'" ++ s ++ "'") args))
                runCabal args handle
            ec2 <- waitForProcess ph1
            unless (ec2 == ExitSuccess) $ do
                putStrLn "Building of build tools failed, please see build-tools.log"
                exitWith ec2
            putStrLn "Build tools built"

    ph <- withBinaryFile "build.log" WriteMode $ \handle -> do
        let args = addCabalArgs settings
                 $ "install"
                 : ("--cabal-lib-version=" ++ libVersion)
                 : "--build-log=logs/$pkg.log"
                 : "-j"
                 : concat
                    [ extraArgs settings
                    , bpPackageList bp
                    ]
        hPutStrLn handle ("cabal " ++ unwords (map (\s -> "'" ++ s ++ "'") args))
        runCabal args handle
    ec <- waitForProcess ph
    unless (ec == ExitSuccess) $ do
        putStrLn "Build failed, please see build.log"
        exitWith ec
