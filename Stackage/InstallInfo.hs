module Stackage.InstallInfo
    ( getInstallInfo
    , iiPackageList
    ) where

import           Control.Arrow            ((&&&))
import           Control.Monad            (forM_)
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import           Data.Version             (showVersion)
import qualified Distribution.Text
import           Distribution.Version     (withinRange, simplifyVersionRange)
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
    putStrLn "Loading Haskell Platform"
    hp <- loadHaskellPlatform settings
    let allPackages'
            | requireHaskellPlatform settings = Map.union (stablePackages settings) $ identsToRanges (hplibs hp)
            | otherwise = stablePackages settings
        allPackages = dropExcluded settings allPackages'
    let totalCore = extraCore settings `Set.union` Set.map (\(PackageIdentifier p _) -> p) (hpcore hp)

    putStrLn "Loading package database"
    pdb <- loadPackageDB settings totalCore allPackages

    putStrLn "Narrowing package database"
    final <- narrowPackageDB settings pdb $ Set.fromList $ Map.toList $ Map.map snd $ allPackages

    putStrLn "Printing build plan to build-plan.log"
    writeFile "build-plan.log" $ unlines $ map showDep $ Map.toList final

    case checkBadVersions settings final of
        badVersions
            | Map.null badVersions -> return ()
            | otherwise -> do
                forM_ (Map.toList badVersions) $ \(user, badDeps) -> do
                    putStrLn $ user ++ " cannot use: "
                    forM_ (Map.toList badDeps) $ \(name, (version, range)) -> do
                        putStrLn $ concat
                            [ "- "
                            , packageVersionString (name, version)
                            , " -- "
                            , Distribution.Text.display $ simplifyVersionRange range
                            ]
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
                 -> Map String (Map PackageName (Version, VersionRange))
checkBadVersions settings buildPlan =
    Map.unions $ map getBadVersions $ Map.toList $ Map.filterWithKey unexpectedFailure buildPlan
  where
    unexpectedFailure name _ = name `Set.notMember` expectedFailures settings

    getBadVersions :: (PackageName, BuildInfo) -> Map String (Map PackageName (Version, VersionRange))
    getBadVersions (name, bi)
        | Map.null badVersions = Map.empty
        | otherwise = Map.singleton display badVersions
      where
        badVersions = Map.unions $ map (uncurry checkPackage) $ Map.toList $ biDeps bi
        display = concat
            [ packageVersionString (name, biVersion bi)
            , " ("
            , unMaintainer $ biMaintainer bi
            , ")"
            ]

    checkPackage :: PackageName -> VersionRange -> Map PackageName (Version, VersionRange)
    checkPackage name vr =
        case Map.lookup name buildPlan of
            -- Can't find the dependency. Could be part of core, so just ignore
            -- it.
            Nothing -> Map.empty
            Just bi
                | biVersion bi `withinRange` vr -> Map.empty
                | otherwise -> Map.singleton name (biVersion bi, vr)
