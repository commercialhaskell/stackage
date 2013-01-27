module Stackage.Build
    ( build
    , defaultBuildSettings
    , BuildSettings (..)
    ) where

import           Control.Exception    (assert)
import           Control.Monad        (unless, when)
import qualified Data.Map             as Map
import           Data.Maybe           (mapMaybe)
import           Data.Set             (empty)
import qualified Data.Set             as Set
import           Distribution.Text    (simpleParse)
import           Distribution.Version (withinRange)
import           Prelude              hiding (pi)
import           Stackage.CheckPlan
import           Stackage.Config
import           Stackage.InstallInfo
import           Stackage.Tarballs
import           Stackage.Test
import           Stackage.Types
import           Stackage.Util
import           System.Directory     (canonicalizePath,
                                       createDirectoryIfMissing,
                                       doesDirectoryExist)
import           System.Exit          (ExitCode (ExitSuccess), exitWith)
import           System.IO            (IOMode (WriteMode), hPutStrLn,
                                       withBinaryFile)
import           System.Process       (rawSystem, readProcess, runProcess,
                                       waitForProcess)
import Stackage.Select (select)
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
            ec1 <- waitForProcess ph1
            unless (ec1 == ExitSuccess) $ do
                putStrLn "Building of build tools failed, please see build-tools.log"
                exitWith ec1
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

-- | Get all of the build tools required.
iiBuildTools :: InstallInfo -> [String]
iiBuildTools InstallInfo { iiPackageDB = PackageDB m, iiPackages = packages } =
    -- FIXME possible improvement: track the dependencies between the build
    -- tools themselves, and install them in the correct order.
    map unPackageName
  $ filter (flip Set.notMember coreTools)
  $ mapMaybe (flip Map.lookup buildToolMap)
  $ Set.toList
  $ Set.unions
  $ map piBuildTools
  $ Map.elems
  $ Map.filterWithKey isSelected m
  where
    unPackageName (PackageName pn) = pn
    isSelected name _ = name `Set.member` selected
    selected = Set.fromList $ Map.keys packages

    -- Build tools shipped with GHC which we should not attempt to build
    -- ourselves.
    coreTools = Set.fromList $ map PackageName $ words "hsc2hs"

    -- The map from build tool name to the package it comes from.
    buildToolMap = Map.unions $ map toBuildToolMap $ Map.toList m
    toBuildToolMap :: (PackageName, PackageInfo) -> Map Executable PackageName
    toBuildToolMap (pn, pi) = Map.unions
                            $ map (flip Map.singleton pn)
                            $ Set.toList
                            $ piExecs pi
