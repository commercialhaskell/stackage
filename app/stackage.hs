{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad
import Data.Monoid
import Data.String (fromString)
import Data.Version
import Options.Applicative
import Filesystem.Path.CurrentOS (decodeString)
import Paths_stackage (version)
import Stackage.CompleteBuild
import Stackage.Upload
import Stackage.InstallBuild
import Network.HTTP.Client (withManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.Text as T

main :: IO ()
main =
    join $
    execParser $
    info
        (helpOption <*> versionOption <*> config)
        (header "Stackage" <>
         fullDesc)
  where
    helpOption =
        abortOption ShowHelpText $
        long "help" <>
        help "Show this help text"
    versionOption =
        infoOption
            ("stackage version " ++ showVersion version)
            (long "version" <>
             help "Show stackage version")
    config =
        subparser $
        mconcat
            [ cmnd
                  (uncurry completeBuild)
                  (fmap (Nightly, ) buildFlags)
                  "nightly"
                  "Build, test and upload the Nightly snapshot"
            , cmnd
                  (uncurry completeBuild)
                  (fmap (LTS Major, ) buildFlags)
                  "lts-major"
                  "Build, test and upload the LTS (major) snapshot"
            , cmnd
                  (uncurry completeBuild)
                  (fmap (LTS Minor, ) buildFlags)
                  "lts-minor"
                  "Build, test and upload the LTS (minor) snapshot"
            , cmnd
                  justUploadNightly
                  nightlyUploadFlags
                  "upload-nightly"
                  "Upload an already-built nightly snapshot"
            , cmnd
                  (const justCheck)
                  (pure ())
                  "check"
                  "Just check that the build plan is ok"
            , cmnd
                  installBuild
                  installFlags
                  "install"
                  "Install a snapshot from an existing build plan"
            , cmnd
                  uploadv2
                  uploadv2Flags
                  "upload2"
                  "Upload a pre-existing v2 bundle"
            ]

    cmnd exec parse name desc =
        command name $
        info
            (fmap exec (parse <**> helpOption))
            (progDesc desc)

    buildFlags =
        BuildFlags <$>
        fmap
            not
            (switch
                 (long "skip-tests" <>
                  help "Skip build and running the test suites")) <*>
        fmap
            not
            (switch
                 (long "skip-haddock" <>
                  help "Skip generating haddock documentation")) <*>
        fmap
            not
            (switch
                 (long "skip-upload" <>
                  help "Skip uploading bundle, docs, etc.")) <*>
        switch
            (long "enable-library-profiling" <>
             help "Enable profiling when building") <*>
        switch
            (long "enable-executable-dynamic" <>
             help "Enable dynamic executables when building") <*>
        switch
            (long "verbose" <> short 'v' <>
             help "Output verbose detail about the build steps") <*>
        switch
            (long "skip-check" <>
             help "Skip the check phase, and pass --allow-newer to cabal configure") <*>
        switch
            (long "upload-v2" <>
             help "Use the V2 upload code")

    nightlyUploadFlags = fromString <$> strArgument
        (metavar "DATE" <>
         help "Date, in YYYY-MM-DD format")

    installFlags =
        InstallFlags <$>
        (fmap
            BPSBundleWeb
            (strOption
                (long "bundle" <>
                 metavar "URL" <>
                 help "Stackage bundle containing build plan")) <|>
         fmap
            (BPSFile . decodeString)
            (strOption
                (long "build-plan" <>
                 metavar "PATH" <>
                 help "Build-plan YAML file"))) <*>
        fmap
            decodeString
            (strArgument
                (metavar "DESTINATION-PATH" <>
                 help "Destination directory path")) <*>
        (fmap
            (Just . decodeString)
            (strOption
                (long "log-dir" <>
                 metavar "PATH" <>
                 help "Location of log files (default DESTINATION-PATH/logs)")) <|>
         pure Nothing) <*>
        option
            auto
            (long "jobs" <>
             metavar "NUMBER" <>
             showDefault <> value 8 <>
             help "Number of threads") <*>
        switch
            (long "global" <>
             help "Install in global package database") <*>
        fmap
            not
            (switch
                (long "skip-tests" <>
                 help "Skip build and running the test suites")) <*>
        fmap
            not
            (switch
                 (long "skip-haddock" <>
                  help "Skip generating haddock documentation")) <*>
        switch
            (long "enable-library-profiling" <>
             help "Enable profiling when building") <*>
        switch
            (long "enable-executable-dynamic" <>
             help "Enable dynamic executables when building") <*>
        switch
            (long "verbose" <> short 'v' <>
             help "Output verbose detail about the build steps") <*>
        switch
            (long "skip-check" <>
             help "Skip the check phase, and pass --allow-newer to cabal configure")

    uploadv2 (path, url) = withManager tlsManagerSettings $ \man -> do
        token <- getStackageAuthToken
        res <- flip uploadBundleV2 man UploadBundleV2
            { ub2AuthToken = token
            , ub2Server = fromString url
            , ub2Bundle = decodeString path
            }
        putStrLn $ "New URL: " ++ T.unpack res

    uploadv2Flags = (,)
        <$> (strArgument
                (metavar "BUNDLE-PATH" <>
                 help "Bundle path"))
        <*> strOption
                (long "server-url" <>
                 metavar "SERVER-URL" <>
                 showDefault <> value (T.unpack $ unStackageServer def) <>
                 help "Server to upload bundle to")
