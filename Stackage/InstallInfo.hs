{-# LANGUAGE RecordWildCards #-}
module Stackage.InstallInfo
    ( getInstallInfo
    , bpPackageList
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

dropExcluded :: SelectSettings
             -> Map PackageName (VersionRange, Maintainer)
             -> Map PackageName (VersionRange, Maintainer)
dropExcluded bs m0 =
    Set.foldl' (flip Map.delete) m0 (excludedPackages bs)

getInstallInfo :: SelectSettings -> IO InstallInfo
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

    case checkBadVersions settings pdb final of
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
        , iiPackages = Map.map biToSPI final
        , iiOptionalCore = Map.fromList $ map (\(PackageIdentifier p v) -> (p, v)) $ Set.toList $ hplibs hp
        , iiPackageDB = pdb
        }

biToSPI :: BuildInfo -> SelectedPackageInfo
biToSPI BuildInfo {..} = SelectedPackageInfo
    { spiVersion = biVersion
    , spiMaintainer = biMaintainer
    , spiGithubUser = biGithubUser
    , spiHasTests = biHasTests
    }

showDep :: (PackageName, BuildInfo) -> String
showDep (PackageName name, BuildInfo {..}) =
    concat
        [ name
        , "-"
        , showVersion biVersion
        , " ("
        , unMaintainer biMaintainer
        , case biGithubUser of
            Nothing -> ""
            Just x -> " @" ++ x
        , ")"
        , ": "
        , unwords $ map unP biUsers
        ]
  where
    unP (PackageName p) = p

bpPackageList :: BuildPlan -> [String]
bpPackageList = map packageVersionString . Map.toList . Map.map spiVersion . bpPackages

-- | Check for internal mismatches in required and actual package versions.
checkBadVersions :: SelectSettings
                 -> PackageDB
                 -> Map PackageName BuildInfo
                 -> Map String (Map PackageName (Version, VersionRange))
checkBadVersions settings (PackageDB pdb) buildPlan =
    Map.unions $ map getBadVersions $ Map.toList $ Map.filterWithKey unexpectedFailure buildPlan
  where
    unexpectedFailure name _ = name `Set.notMember` expectedFailuresSelect settings

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
            , case Map.lookup name pdb of
                Just PackageInfo { piGithubUser = Just gu } -> " @" ++ gu
                _ -> ""
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
