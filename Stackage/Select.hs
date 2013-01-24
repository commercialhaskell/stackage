module Stackage.Select
    ( select
    , defaultSelectSettings
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
import Stackage.BuildPlan

defaultSelectSettings :: SelectSettings
defaultSelectSettings = SelectSettings
    { extraCore = defaultExtraCore
    , expectedFailuresSelect = defaultExpectedFailures
    , stablePackages = defaultStablePackages
    , haskellPlatformCabal = "haskell-platform/haskell-platform.cabal"
    , requireHaskellPlatform = True
    , excludedPackages = empty
    , flags = Set.fromList $ words "blaze_html_0_5"
    , allowedPackage = const $ Right ()
    }

select :: SelectSettings -> IO BuildPlan
select settings' = do
    ii <- getInstallInfo settings'

    return BuildPlan
        { bpTools = iiBuildTools ii
        , bpPackages = iiPackages ii
        , bpOptionalCore = iiOptionalCore ii
        , bpCore = iiCore ii
        }

-- | Get all of the build tools required.
iiBuildTools :: InstallInfo -> [String]
iiBuildTools InstallInfo { iiPackageDB = PackageDB m, iiPackages = packages } =
    -- FIXME possible improvement: track the dependencies between the build
    -- tools themselves, and install them in the correct order.
    map packageVersionString
  $ filter (flip Set.notMember coreTools . fst)
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
    toBuildToolMap :: (PackageName, PackageInfo) -> Map Executable (PackageName, Version)
    toBuildToolMap (pn, pi) = Map.unions
                            $ map (flip Map.singleton (pn, piVersion pi))
                            $ Set.toList
                            $ piExecs pi
