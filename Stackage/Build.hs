module Stackage.Build
    ( build
    , defaultBuildSettings
    , BuildSettings (..)
    ) where

import           Control.Exception    (assert)
import           Control.Monad        (unless, when)
import qualified Data.Map             as Map
import           Data.Set             (empty)
import qualified Data.Set             as Set
import           Distribution.Text    (simpleParse)
import           Distribution.Version (thisVersion, withinRange)
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

defaultBuildSettings :: BuildSettings
defaultBuildSettings = BuildSettings
    { sandboxRoot = "sandbox"
    , extraBuildArgs = []
    , extraCore = defaultExtraCore
    , expectedFailures = defaultExpectedFailures
    , stablePackages = defaultStablePackages
    , extraArgs = ["-fnetwork23"]
    , haskellPlatformCabal = "haskell-platform/haskell-platform.cabal"
    , requireHaskellPlatform = True
    , cleanBeforeBuild = True
    , excludedPackages = empty
    }

build :: BuildSettings -> IO ()
build settings' = do
    putStrLn "Creating a build plan"
    ii <- getInstallInfo settings'

    let root' = sandboxRoot settings'
    initPkgDb <- if cleanBeforeBuild settings'
       then do
         putStrLn "Wiping out old sandbox folder"
         rm_r root'
         rm_r "logs"
         return True
       else do
         b <- doesDirectoryExist root'
         when b (putStrLn "Re-using existing sandbox")
         return (not b)
    createDirectoryIfMissing True root'
    root <- canonicalizePath root'
    let settings = settings' { sandboxRoot = root }

    when initPkgDb $ do
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

    menv <- fmap Just $ getModifiedEnv settings
    let runCabal args handle = runProcess "cabal" args Nothing menv Nothing (Just handle) (Just handle)

    -- First install build tools so they can be used below.
    case iiBuildTools ii of
        [] -> putStrLn "No build tools required"
        tools -> do
            putStrLn $ "Installing the following build tools: " ++ unwords tools
            ph1 <- withBinaryFile "build-tools.log" WriteMode $ \handle -> do
                let args = addCabalArgs settings
                         $ "install"
                         : ("--cabal-lib-version=" ++ libVersion)
                         : "--build-log=logs-tools/$pkg.log"
                         : "-j"
                         : iiBuildTools ii
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
                    [ extraBuildArgs settings
                    , extraArgs settings
                    , iiPackageList ii
                    ]
        hPutStrLn handle ("cabal " ++ unwords (map (\s -> "'" ++ s ++ "'") args))
        runCabal args handle
    ec <- waitForProcess ph
    unless (ec == ExitSuccess) $ do
        putStrLn "Build failed, please see build.log"
        exitWith ec

    putStrLn "Sandbox built, beginning individual test suites"
    runTestSuites settings ii

    putStrLn "All test suites that were expected to pass did pass, building tarballs."
    makeTarballs ii

-- | Get all of the build tools required.
iiBuildTools :: InstallInfo -> [String]
iiBuildTools InstallInfo { iiPackageDB = PackageDB m, iiPackages = packages } =
    -- FIXME possible improvement: track the dependencies between the build
    -- tools themselves, and install them in the correct order.
    map unPackageName
  $ filter (flip Set.notMember coreTools)
  $ filter (flip Map.member m)
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
