{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad
import Data.Monoid
import Data.Version
import Options.Applicative
import Paths_stackage (version)
import Stackage.CompleteBuild

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
            ("fpbuild version " ++ showVersion version)
            (long "version" <>
             help "Show fpbuild version")
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
                  (const justCheck)
                  (pure ())
                  "check"
                  "Just check that the build plan is ok"]
    cmnd exec parse name desc =
        command name $
        info
            (fmap exec parse)
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
                 (long "skip-upload" <>
                  help "Skip uploading bundle, docs, etc.")) <*>
        switch
            (long "enable-library-profiling" <>
             help "Enable profiling when building") <*>
        switch
            (long "verbose" <> short 'v' <>
             help "Output verbose detail about the build steps")
