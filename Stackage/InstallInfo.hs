module Stackage.InstallInfo
    ( getInstallInfo
    , iiPackageList
    ) where

import           Control.Arrow            ((&&&))
import           Control.Monad            (forM_)
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import           Data.Version             (showVersion)
import           Distribution.Version     (withinRange)
import           Stackage.Config
import           Stackage.HaskellPlatform
import           Stackage.LoadDatabase
import           Stackage.NarrowDatabase
import           Stackage.Types
import           Stackage.Util

dropExcluded :: BuildSettings
             -> Map PackageName (VersionRange, Maintainer)
             -> Map PackageName (VersionRange, Maintainer)
dropExcluded bs m0 =
    Set.foldl' (flip Map.delete) m0 (excludedPackages bs)

getInstallInfo :: BuildSettings -> IO InstallInfo
getInstallInfo settings = do
    hp <- loadHaskellPlatform settings
    let allPackages'
            | requireHaskellPlatform settings = Map.union (stablePackages settings) $ identsToRanges (hplibs hp)
            | otherwise = stablePackages settings
        allPackages = dropExcluded settings allPackages'
    let totalCore = extraCore settings `Set.union` Set.map (\(PackageIdentifier p _) -> p) (hpcore hp)
    pdb <- loadPackageDB totalCore allPackages
    final <- narrowPackageDB pdb $ Set.fromList $ Map.toList $ Map.map snd $ allPackages

    putStrLn "Printing build plan to build-plan.log"
    writeFile "build-plan.log" $ unlines $ map showDep $ Map.toList final

    case checkBadVersions settings final of
        badVersions
            | Map.null badVersions -> return ()
            | otherwise -> do
                forM_ (Map.toList badVersions) $ \(PackageName user, badDeps) -> do
                    putStrLn $ user ++ " cannot use: "
                    mapM_ (putStrLn . packageVersionString) $ Map.toList badDeps
                    putStrLn ""

                error "Conflicting build plan, exiting"

    return InstallInfo
        { iiCore = totalCore
        , iiPackages = Map.map (biVersion &&& biMaintainer) final
        , iiOptionalCore = Map.fromList $ map (\(PackageIdentifier p v) -> (p, v)) $ Set.toList $ hplibs hp
        , iiPackageDB = pdb
        }

showDep :: (PackageName, BuildInfo) -> String
showDep (PackageName name, (BuildInfo version deps (Maintainer m) _)) =
    concat
        [ name
        , "-"
        , showVersion version
        , " ("
        , m
        , ")"
        , ": "
        , unwords $ map unP deps
        ]
  where
    unP (PackageName p) = p

iiPackageList :: InstallInfo -> [String]
iiPackageList = map packageVersionString . Map.toList . Map.map fst . iiPackages

-- | Check for internal mismatches in required and actual package versions.
checkBadVersions :: BuildSettings
                 -> Map PackageName BuildInfo
                 -> Map PackageName (Map PackageName Version)
checkBadVersions settings buildPlan =
    Map.filter (not . Map.null) $ Map.map getBadVersions $ Map.filterWithKey unexpectedFailure buildPlan
  where
    unexpectedFailure name _ = name `Set.notMember` expectedFailures settings

    getBadVersions :: BuildInfo -> Map PackageName Version
    getBadVersions = Map.unions . map (uncurry checkPackage) . Map.toList . biDeps

    checkPackage :: PackageName -> VersionRange -> Map PackageName Version
    checkPackage name vr =
        case Map.lookup name buildPlan of
            -- Can't find the dependency. Could be part of core, so just ignore
            -- it.
            Nothing -> Map.empty
            Just bi
                | biVersion bi `withinRange` vr -> Map.empty
                | otherwise -> Map.singleton name $ biVersion bi
