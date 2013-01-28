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
    let installBuildTool tool = do
            putStrLn $ "Installing build tool: " ++ tool
            ec <- withBinaryFile "build-tools.log" WriteMode $ \handle -> do
                let args = addCabalArgs settings
                         $ "install"
                         : ("--cabal-lib-version=" ++ libVersion)
                         : "--build-log=logs-tools/$pkg.log"
                         : "-j"
                         : concat
                            [ extraArgs settings
                            , [tool]
                            ]
                hPutStrLn handle ("cabal " ++ unwords (map (\s -> "'" ++ s ++ "'") args))
                ph <- runCabal args handle
                waitForProcess ph
            unless (ec == ExitSuccess) $ do
                putStrLn $ concat
                    [ "Building of "
                    , tool
                    , " failed, please see build-tools.log"
                    ]
                exitWith ec
            putStrLn $ tool ++ " built"
    mapM_ installBuildTool $ bpTools bp

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
