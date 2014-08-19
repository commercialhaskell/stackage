{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Stackage.InstallInfo
    ( getInstallInfo
    , bpPackageList
    ) where

import           Control.Monad            (forM_, unless)
import           Data.List                (foldl')
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import           Data.Time                (getCurrentTime, formatTime)
import           Data.Version             (showVersion)
import qualified Distribution.Text
import           Distribution.Version     (simplifyVersionRange, withinRange)
import           Stackage.GhcPkg
import           Stackage.LoadDatabase
import           Stackage.NarrowDatabase
import           Stackage.ServerFiles
import           Stackage.Types
import           Stackage.Util
import           System.Directory         (createDirectoryIfMissing)
import           System.FilePath          ((</>))
import qualified System.IO                as IO
import qualified System.IO.UTF8
import           System.Locale            (defaultTimeLocale)
import           System.Exit              (exitFailure)

dropExcluded :: SelectSettings
             -> Map PackageName (VersionRange, Maintainer)
             -> Map PackageName (VersionRange, Maintainer)
dropExcluded bs m0 =
    foldl' (flip Map.delete) m0 (Set.toList $ excludedPackages bs)

getInstallInfo :: SelectSettings -> IO InstallInfo
getInstallInfo settings = do
    core <- do
        putStrLn "Loading core packages from global database"
        getGlobalPackages $ selectGhcVersion settings
    underlay <- getDBPackages (selectUnderlayPackageDirs settings) (selectGhcVersion settings)
    let underlaySet = Set.map pkgName underlay
        coreMap = Map.unions
                $ map (\(PackageIdentifier k v) -> Map.singleton k v)
                $ Set.toList core
        allPackages' =
            stablePackages settings $ requireHaskellPlatform settings
        allPackages = dropExcluded settings allPackages'
        totalCore
            | ignoreUpgradeableCore settings =
                Map.fromList $ map (\n -> (PackageName n, Nothing)) $ words "base containers template-haskell"
            | otherwise =
                Map.fromList (map (\(PackageIdentifier p v) -> (p, Just v)) (Set.toList core))
                `Map.union` Map.fromList (map (, Nothing) (Set.toList $ extraCore settings))

    putStrLn "Loading package database"
    pdb <- loadPackageDB settings coreMap (Map.keysSet totalCore) allPackages underlaySet

    putStrLn "Narrowing package database"
    (final, errs) <- narrowPackageDB settings (Map.keysSet totalCore) pdb $ Set.fromList $ Map.toList $ Map.map snd $ allPackages

    putStrLn "Printing build plan to build-plan.log"
    System.IO.UTF8.writeFile "build-plan.log" $ unlines $ map showDep $ Map.toList final
    System.IO.UTF8.writeFile "hackage-map.txt" $ unlines $ map showHackageMap $ Map.toList final

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

    let ii = InstallInfo
            { iiCore = totalCore
            , iiPackages = Map.map biToSPI final
            , iiOptionalCore = Map.empty
            , iiPackageDB = pdb
            }

    forM_ [False, True] $ \isInc -> do
        let incexc = if isInc then "inclusive" else "exclusive"

        now <- getCurrentTime
        let ghcVer =
                let GhcMajorVersion x y = selectGhcVersion settings
                 in show x ++ "." ++ show y
            date = formatTime defaultTimeLocale "%Y-%m-%d" now

        createDirectoryIfMissing True incexc

        putStrLn $ "Inclusive/exclusive: " ++ incexc

        putStrLn "Creating hackage file (for publishing to Stackage server)"
        let isHP = requireHaskellPlatform settings
        IO.withBinaryFile (incexc </> "hackage") IO.WriteMode $ \hackageH ->
            IO.withBinaryFile (incexc </> "create-snapshot.sh") IO.WriteMode
            (createHackageFile isInc isHP ii ghcVer date hackageH)

        putStrLn "Creating desc file (for publishing to Stackage server)"
        System.IO.UTF8.writeFile (incexc </> "desc") $ concat
            [ "Stackage build for GHC "
            , ghcVer
            , if requireHaskellPlatform settings
                then " + Haskell Platform"
                else ""
            , ", "
            , date
            , ", "
            , incexc
            , "\nGenerated on "
            , show now
            ]

    return ii

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
        , " " ++ githubMentions biGithubUser
        , ")"
        , ": "
        , unwords $ map unP biUsers
        ]
  where
    unP (PackageName p) = p

-- | Convert to format used by Hackage for displaying distribution versions.
-- For more info, see https://github.com/fpco/stackage/issues/38.
showHackageMap :: (PackageName, BuildInfo) -> String
showHackageMap (PackageName name, BuildInfo {..}) =
    show (name, showVersion biVersion, Nothing :: Maybe String)

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
                Just PackageInfo { piGithubUser = gus } -> " " ++ githubMentions gus
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
