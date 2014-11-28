{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE CPP #-}
module Stackage.Select
    ( select
    , defaultSelectSettings
    ) where

import           Data.Either          (partitionEithers)
import qualified Data.Map             as Map
import           Data.Maybe           (mapMaybe)
import           Data.Set             (empty)
import qualified Data.Set             as Set
import           Distribution.Text    (simpleParse)
import           Distribution.Version (withinRange)
import           Prelude              hiding (pi)
import           Stackage.Config
import           Stackage.InstallInfo
import           Stackage.Types
import           Stackage.Util

defaultSelectSettings :: GhcMajorVersion
                      -> Bool -- ^ haskell platform?
                      -> SelectSettings
defaultSelectSettings version requireHP = SelectSettings
    { extraCore = defaultExtraCore version
    , expectedFailures = defaultExpectedFailures version requireHP
    , stablePackages = defaultStablePackages version
    , haskellPlatformDir = "hp"
    , requireHaskellPlatform = requireHP
    , ignoreUpgradeableCore = False
    , excludedPackages = empty
    , flags = \coreMap ->
        Set.fromList (words "blaze_html_0_5 small_base https splitbase old-locale") `Set.union`

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
        -- Needed on Windows to get unix-compat to compile
        (if version >= GhcMajorVersion 7 6 then Set.empty else Set.fromList (words "old-time"))
            `Set.union`
#endif
        -- Support for containers-unicode-symbols
        (case Map.lookup (PackageName "containers") coreMap of
            Just v | Just range <- simpleParse "< 0.5", v `withinRange` range
                -> Set.singleton "containers-old"
            _ -> Set.empty) `Set.union`

        -- Support for network 2.6
        (if version >= GhcMajorVersion 7 8 && not requireHP
            then Set.singleton "network-uri"
            else Set.empty)
    , disabledFlags = Set.fromList (words "bytestring-in-base test-hlint")
        `Set.union`
        (if version <= GhcMajorVersion 7 4
            then Set.singleton "bytestring-builder"
            else Set.empty)

        -- SHA and binary

        `Set.union`
        (if version <= GhcMajorVersion 7 6
            then Set.singleton "decoderinterface"
            else Set.empty) `Set.union`

        -- Support for network 2.6
        (if version >= GhcMajorVersion 7 8 && not requireHP
            then Set.empty
            else Set.singleton "network-uri")
    , allowedPackage = const $ Right ()
    , useGlobalDatabase = False
    , skippedTests =
        Set.insert (PackageName "ReadArgs") $ -- old version of hspec
        if version >= GhcMajorVersion 7 8
            then Set.fromList
                    [ PackageName "punycode" -- pulls in encoding
                    ]
            else Set.empty
    , selectGhcVersion = version
    , selectTarballDir = "patching/tarballs"
    , selectUnderlayPackageDirs = []
    }

select :: SelectSettings -> IO BuildPlan
select settings' = do
    ii <- getInstallInfo settings'

    bt <-
        case iiBuildTools ii of
            Left s -> error $ "Could not topologically sort build tools: " ++ s
            Right x -> return x

    return BuildPlan
        { bpTools = bt
        , bpPackages = iiPackages ii
        , bpOptionalCore = iiOptionalCore ii
        , bpCore = iiCore ii
        , bpSkippedTests = skippedTests settings'
        , bpExpectedFailures = expectedFailures settings'
        }

-- | Get all of the build tools required.
iiBuildTools :: InstallInfo -> Either String [String]
iiBuildTools InstallInfo { iiPackageDB = PackageDB m, iiPackages = packages } =
    fmap (map packageVersionString)
  $ topSort
  $ map addDependencies
  $ filter (flip Set.notMember coreTools . fst)
  $ Set.toList
  $ Set.fromList
  $ mapMaybe (flip Map.lookup buildToolMap)
  $ Set.toList
  $ Set.unions
  $ map piBuildToolsAll
  $ Map.elems
  $ Map.filterWithKey isSelected m
  where
    isSelected name _ = name `Set.member` selected
    selected = Set.fromList $ Map.keys packages

    -- Build tools shipped with GHC which we should not attempt to build
    -- ourselves.
    coreTools = Set.fromList $ map PackageName $ words "hsc2hs"

    -- The map from build tool name to the package it comes from.
    buildToolMap :: Map Executable (PackageName, Version)
    buildToolMap = Map.unions $ map toBuildToolMap $ Map.toList m

    toBuildToolMap :: (PackageName, PackageInfo) -> Map Executable (PackageName, Version)
    toBuildToolMap (pn, pi) = Map.unions
                            $ map (flip Map.singleton (pn, piVersion pi))
                            $ Set.toList
                            $ piExecs pi

    addDependencies :: (PackageName, Version) -> ((PackageName, Version), Set (PackageName, Version))
    addDependencies (pn, pv) =
        ((pn, pv), deps)
      where
        deps =
            case Map.lookup pn m of
                Nothing -> Set.empty
                Just pi -> Set.fromList
                         $ mapMaybe (flip Map.lookup buildToolMap)
                         $ Set.toList
                         $ piBuildToolsExe pi `Set.union` manualDeps

        manualDeps
            | pn == PackageName "c2hs" = Set.singleton $ Executable "happy"
            | otherwise = Set.empty

topSort :: (Show a, Ord a) => [(a, Set a)] -> Either String [a]
topSort orig =
    uncurry go . partitionEithers . map (splitter . limitDeps) $ orig
  where
    splitter (x, y)
        | Set.null y = Left x
        | otherwise = Right (x, y)

    go x [] = Right x
    go [] y = Left $ "The following form a cycle: " ++ show (map fst y)
    go (x:xs) ys = do
        let (xs', ys') = partitionEithers $ map (splitter . dropDep x) ys
        rest <- go (xs ++ xs') ys'
        return $ x : rest

    dropDep x (y, z) = (y, Set.delete x z)

    allVertices = Set.fromList $ map fst orig
    limitDeps (x, y) = (x, Set.intersection allVertices y)
