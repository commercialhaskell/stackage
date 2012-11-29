module Stackage.Build
    ( build
    ) where

import           Distribution.Text       (simpleParse)
import           Control.Monad        (unless)
import           Stackage.CheckPlan
import           Stackage.InstallInfo
import           Stackage.Tarballs
import           Stackage.Test
import           Stackage.Util
import           System.Exit          (ExitCode (ExitSuccess), exitWith)
import           System.IO            (IOMode (WriteMode), withBinaryFile)
import           System.Process       (runProcess, waitForProcess, rawSystem, readProcess)
import           System.Directory     (createDirectoryIfMissing, canonicalizePath)
import           Distribution.Version    (thisVersion, withinRange)
import Control.Exception (assert)

build :: FilePath
      -> ([String] -> [String]) -- ^ extra build rgs
      -> IO ()
build root' extraBuildArgs = do
    putStrLn "Creating a build plan"
    ii <- getInstallInfo

    putStrLn "Wiping out old sandbox folder"
    rm_r root'
    rm_r "logs"
    createDirectoryIfMissing True root'
    root <- canonicalizePath root'

    ec1 <- rawSystem "ghc-pkg" ["init", packageDir root]
    unless (ec1 == ExitSuccess) $ do
        putStrLn "Unable to create package database via ghc-pkg init"
        exitWith ec1

    let extraArgs = ("-fnetwork23":)

    checkPlan (addCabalArgs root . extraArgs) ii
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
        let args = addCabalArgs root
                 $ "install"
                 : ("--cabal-lib-version=" ++ libVersion)
                 : "--build-log=logs/$pkg.log"
                 : "--enable-shared"
                 : "-j"
                 : (extraBuildArgs . extraArgs) (iiPackageList ii)
         in runProcess "cabal" args Nothing Nothing Nothing (Just handle) (Just handle)
    ec <- waitForProcess ph
    unless (ec == ExitSuccess) $ do
        putStrLn "Build failed, please see build.log"
        exitWith ec

    putStrLn "Sandbox built, beginning individual test suites"
    runTestSuites root ii

    putStrLn "All test suites that were expected to pass did pass, building tarballs."
    makeTarballs ii
