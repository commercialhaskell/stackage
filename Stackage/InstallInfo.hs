{-# LANGUAGE RecordWildCards #-}
module Stackage.InstallInfo
    ( getInstallInfo
    , bpPackageList
    ) where

import           Control.Monad            (forM_, unless)
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import           Data.Version             (showVersion)
import qualified Distribution.Text
import           Distribution.Version     (simplifyVersionRange, withinRange)
import           Stackage.GhcPkg
import           Stackage.HaskellPlatform
import           Stackage.LoadDatabase
import           Stackage.NarrowDatabase
import           Stackage.Types
import           Stackage.Util
import qualified System.IO.UTF8
import           System.Exit              (exitFailure)

dropExcluded :: SelectSettings
             -> Map PackageName (VersionRange, Maintainer)
             -> Map PackageName (VersionRange, Maintainer)
dropExcluded bs m0 =
    Set.foldl' (flip Map.delete) m0 (excludedPackages bs)

getInstallInfo :: SelectSettings -> IO InstallInfo
getInstallInfo settings = do
    putStrLn "Loading Haskell Platform"
    mhp <- loadHaskellPlatform settings

    core <-
        case mhp of
            Just hp | not (useGlobalDatabase settings) -> return $ hpcore hp
            _ -> do
                putStrLn "Loading core packages from global database"
                getGlobalPackages $ selectGhcVersion settings
    let coreMap = Map.unions
                $ map (\(PackageIdentifier k v) -> Map.singleton k v)
                $ Set.toList core

    let allPackages' =
            case mhp of
                Just hp | requireHaskellPlatform settings ->
                    Map.union (stablePackages settings) $ identsToRanges (hplibs hp)
                _ -> stablePackages settings
        allPackages = dropExcluded settings allPackages'
    let totalCore = extraCore settings `Set.union` Set.map (\(PackageIdentifier p _) -> p) core

    putStrLn "Loading package database"
    pdb <- loadPackageDB settings coreMap totalCore allPackages

    putStrLn "Narrowing package database"
    (final, errs) <- narrowPackageDB settings totalCore pdb $ Set.fromList $ Map.toList $ Map.map snd $ allPackages

    putStrLn "Printing build plan to build-plan.log"
    System.IO.UTF8.writeFile "build-plan.log" $ unlines $ map showDep $ Map.toList final

    unless (Set.null errs) $ do
        putStrLn "Build plan requires some disallowed packages"
        mapM_ putStrLn $ Set.toList errs
        exitFailure

    putStrLn "Checking for bad versions"
    case checkBadVersions settings coreMap pdb final of
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
        , iiOptionalCore = maybe
            Map.empty
            (Map.fromList . map (\(PackageIdentifier p v) -> (p, v)) . Set.toList . hplibs)
            mhp
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
                 -> Map PackageName Version -- ^ core
                 -> PackageDB
                 -> Map PackageName BuildInfo
                 -> Map String (Map PackageName (Version, VersionRange))
checkBadVersions settings core (PackageDB pdb) buildPlan =
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
            Nothing ->
                case Map.lookup name core of
                    -- Might be part of extra-core
                    Nothing -> Map.empty
                    Just version
                        | version `withinRange` vr -> Map.empty
                        | otherwise -> Map.singleton name (version, vr)
            Just bi
                | biVersion bi `withinRange` vr -> Map.empty
                | otherwise -> Map.singleton name (biVersion bi, vr)
