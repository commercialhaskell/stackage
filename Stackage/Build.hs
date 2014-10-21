module Stackage.Build
    ( build
    , defaultBuildSettings
    , BuildSettings (..)
    ) where

import           Control.Monad              (unless)
import           Prelude                    hiding (pi)
import           Stackage.CheckCabalVersion (checkCabalVersion)
import           Stackage.Config
import           Stackage.InstallInfo
import           Stackage.ModuleNameConflict
import           Stackage.Types
import           Stackage.Util
import           System.Exit                (ExitCode (ExitSuccess), exitWith)
import           System.IO                  (BufferMode (NoBuffering),
                                             IOMode (WriteMode), hPutStrLn,
                                             hSetBuffering, withBinaryFile)
import qualified System.IO.UTF8
import           System.Process             (rawSystem, runProcess,
                                             waitForProcess)
import qualified Data.ByteString.Lazy.Char8 as L8

defaultBuildSettings :: Maybe Int -- ^ argument to -j
                     -> GhcMajorVersion
                     -> BuildSettings
defaultBuildSettings cores version = BuildSettings
    { sandboxRoot = "sandbox"
    , extraArgs = \bs -> "-fnetwork23" : "-fhttps" :
        case bs of
            BSTest -> []
            _ ->
                case cores of
                    Nothing -> ["-j"]
                    Just 1 -> []
                    Just j -> ["-j" ++ show j]
    , testWorkerThreads = 4
    , buildDocs = True
    , tarballDir = "patching/tarballs"
    , cabalFileDir = Nothing
    , underlayPackageDirs = []
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
            let toolsDir = packageDir settings ++ "-tools"
            rm_r toolsDir
            ecInit <- rawSystem "ghc-pkg" ["init", toolsDir]
            unless (ecInit == ExitSuccess) $ do
                putStrLn "Unable to create package database via ghc-pkg init"
                exitWith ecInit

            putStrLn $ "Installing build tool: " ++ tool
            ec <- withBinaryFile "build-tools.log" WriteMode $ \handle -> do
                hSetBuffering handle NoBuffering

                let args = addCabalArgs settings BSTools
                         $ "install"
                         : ("--cabal-lib-version=" ++ libVersion)
                         : "--build-log=logs-tools/$pkg.log"
                         : [tool]
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
            rm_r toolsDir
    mapM_ installBuildTool $ bpTools bp

    putStrLn "Beginning Stackage build"
    ph <- withBinaryFile "build.log" WriteMode $ \handle -> do
        packageList <- mapM (replaceTarball $ tarballDir settings) $ bpPackageList bp
        let args = addCabalArgs settings BSBuild
                 $ "install"
                 : ("--cabal-lib-version=" ++ libVersion)
                 : "--build-log=logs/$pkg.log"
                 : "--max-backjumps=-1"
                 : "--reorder-goals"
                 : packageList
        hPutStrLn handle ("cabal " ++ unwords (map (\s -> "'" ++ s ++ "'") args))
        runCabal args handle
    ec <- waitForProcess ph
    unless (ec == ExitSuccess) $ do
        putStrLn "Build failed, please see build.log"
        L8.readFile "build.log" >>= L8.putStr
        exitWith ec

    putStrLn "Build completed successfully, checking for module name conflicts"
    conflicts <- getModuleNameConflicts $ packageDir settings
    System.IO.UTF8.writeFile "module-name-conflicts.txt"
        $ renderModuleNameConflicts conflicts
