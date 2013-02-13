module Stackage.Select
    ( select
    , defaultSelectSettings
    ) where

import           Data.Either          (partitionEithers)
import qualified Data.Map             as Map
import           Data.Maybe           (mapMaybe)
import           Data.Set             (empty)
import qualified Data.Set             as Set
import           Prelude              hiding (pi)
import           Stackage.Config
import           Stackage.InstallInfo
import           Stackage.Types
import           Stackage.Util

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
    , useGlobalDatabase = False
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
  $ map piBuildTools
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
                         $ piBuildTools pi

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
