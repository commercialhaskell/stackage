{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
-- | Representation of a concrete build plan, and how to generate a new one
-- based on constraints.
module Stackage2.BuildPlan
    ( BuildPlan (..)
    , PackageBuild (..)
    , newBuildPlan
    ) where

import Distribution.Package            (Dependency (..))
import Distribution.PackageDescription
import Distribution.Version            (withinRange, intersectVersionRanges)
import Stackage2.CorePackages
import Stackage2.PackageConstraints
import Stackage2.PackageIndex
import Stackage2.Prelude
import Stackage2.GithubPings
import Control.Monad.State.Strict (execState, get, put)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Aeson

data BuildPlan desc = BuildPlan
    { bpCore        :: Map PackageName Version
    , bpTools       :: Vector (PackageName, Version)
    , bpExtra       :: Map PackageName (PackageBuild desc)
    , bpGlobalFlags :: Map FlagName Bool
    }
    deriving (Functor, Foldable, Traversable, Show, Eq)
type instance Element (BuildPlan desc) = desc
instance MonoFunctor (BuildPlan desc)
instance MonoFoldable (BuildPlan desc)
instance MonoTraversable (BuildPlan desc)

instance ToJSON (BuildPlan desc) where
    toJSON BuildPlan {..} = object
        [ "core" .= asMap (mapFromList $ map toCore $ mapToList bpCore)
        , "tools" .= map goTool bpTools
        , "extra" .= Map.mapKeysWith const (unPackageName) bpExtra
        , "global-flags" .= Map.mapKeysWith const (\(FlagName f) -> f) bpGlobalFlags
        ]
      where
        toCore (x, y) = (asText $ display x, asText $ display y)
        goTool (name, version) = object
            [ "name" .= asText (display name)
            , "version" .= asText (display version)
            ]
instance desc ~ () => FromJSON (BuildPlan desc) where
    parseJSON = withObject "BuildPlan" $ \o -> BuildPlan
        <$> ((o .: "core") >>= goCore)
        <*> ((o .: "tools") >>= mapM goTool)
        <*> (goExtra <$> (o .: "extra"))
        <*> (goFlags <$> (o .: "global-flags"))
      where
        goCore =
            fmap mapFromList . mapM goCore' . mapToList . asHashMap
          where
            goCore' (k, v) = do
                k' <- either (fail . show) return $ simpleParse $ asText k
                v' <- either (fail . show) return $ simpleParse $ asText v
                return (k', v')

        goTool = withObject "Tool" $ \o -> (,)
            <$> ((o .: "name") >>=
                either (fail . show) return . simpleParse . asText)
            <*> ((o .: "version") >>=
                either (fail . show) return . simpleParse . asText)

        goExtra = Map.mapKeysWith const PackageName
        goFlags = Map.mapKeysWith const FlagName

data PackageBuild desc = PackageBuild
    { pbVersion           :: Version
    , pbMaintainer        :: Maybe Maintainer
    , pbGithubPings       :: Set Text
    , pbUsers             :: Set PackageName
    , pbFlags             :: Map FlagName Bool
    , pbTestState         :: TestState
    , pbHaddockState      :: TestState
    , pbTryBuildBenchmark :: Bool
    , pbDesc              :: desc
    }
    deriving (Functor, Foldable, Traversable, Show, Eq)
type instance Element (PackageBuild desc) = desc
instance MonoFunctor (PackageBuild desc)
instance MonoFoldable (PackageBuild desc)
instance MonoTraversable (PackageBuild desc)

instance ToJSON (PackageBuild desc) where
    toJSON PackageBuild {..} = object $ concat
        [ maybe [] (\m -> ["maintainer" .= m]) pbMaintainer
        ,
            [ "version" .= asText (display pbVersion)
            , "github-pings" .= pbGithubPings
            , "users" .= map unPackageName (unpack pbUsers)
            , "flags" .= Map.mapKeysWith const (\(FlagName f) -> asText $ pack f) pbFlags
            , "test-state" .= pbTestState
            , "haddock-state" .= pbHaddockState
            , "build-benchmark" .= pbTryBuildBenchmark
            ]
        ]
instance desc ~ () => FromJSON (PackageBuild desc) where
    parseJSON = withObject "PackageBuild" $ \o -> PackageBuild
        <$> (o .: "version" >>= efail . simpleParse . asText)
        <*> o .:? "maintainer"
        <*> o .:? "github-pings" .!= mempty
        <*> (Set.map PackageName <$> (o .:? "users" .!= mempty))
        <*> (toFlags <$> (o .:? "flags" .!= mempty))
        <*> o .: "test-state"
        <*> o .: "haddock-state"
        <*> o .: "build-benchmark"
        <*> pure ()
      where
        toFlags = Map.mapKeysWith const (FlagName . unpack . asText)

        efail = either (fail . show) return

data TestState = ExpectSuccess
               | ExpectFailure
               | Don'tBuild -- ^ when the test suite will pull in things we don't want
    deriving (Show, Eq, Ord, Bounded, Enum)

testStateToText :: TestState -> Text
testStateToText ExpectSuccess = "expect-success"
testStateToText ExpectFailure = "expect-failure"
testStateToText Don'tBuild    = "do-not-build"

instance ToJSON TestState where
    toJSON = toJSON . testStateToText
instance FromJSON TestState where
    parseJSON = withText "TestState" $ \t ->
        case lookup t states of
            Nothing -> fail $ "Invalid state: " ++ unpack t
            Just v -> return v
      where
        states = asHashMap $ mapFromList
               $ map (\x -> (testStateToText x, x)) [minBound..maxBound]

newBuildPlan :: MonadIO m => m (BuildPlan FlatComponent)
newBuildPlan = liftIO $ do
    core <- getCorePackages
    extraOrig <- getLatestDescriptions (isAllowed core) simplifyDesc
    let toolNames = concatMap (seTools . fcExtra . pbDesc) extraOrig
        extra = populateUsers $ removeUnincluded (Map.keysSet toolNames) extraOrig
    return BuildPlan
        { bpCore = core
        , bpTools = topologicalSort
                  $ filter (\(x, _) -> x `member` toolNames)
                  $ mapToList extra
        , bpExtra = extra
        , bpGlobalFlags = defaultGlobalFlags
        }

topologicalSort :: [(PackageName, PackageBuild FlatComponent)]
                -> Vector (PackageName, Version)
topologicalSort = fromList . fmap (fmap pbVersion) -- FIXME

removeUnincluded :: Set PackageName -- ^ tool names
                 -> Map PackageName (PackageBuild FlatComponent)
                 -> Map PackageName (PackageBuild FlatComponent)
removeUnincluded toolNames orig =
    mapFromList $ filter (\(x, _) -> x `member` included) $ mapToList orig
  where
    included :: Set PackageName
    included = flip execState mempty $ do
        mapM_ (add . fst) $ mapToList $ pcPackages defaultPackageConstraints
        mapM_ add toolNames

    add name = do
        inc <- get
        when (name `notMember` inc) $ do
            put $ insertSet name inc
            case lookup name orig of
                Nothing -> return ()
                Just pb -> mapM_ (add . fst) $ mapToList $ fcDeps $ pbDesc pb

populateUsers :: Map PackageName (PackageBuild FlatComponent)
              -> Map PackageName (PackageBuild FlatComponent)
populateUsers orig =
    mapWithKey go orig
  where
    go name pb = pb { pbUsers = foldMap (go2 name) (mapToList orig) }

    go2 dep (user, pb)
        | dep `member` fcDeps (pbDesc pb) = singletonSet user
        | otherwise = mempty

data SimpleTree = SimpleTree
    { stDeps :: Map PackageName VersionRange
    , stConds :: [(Condition ConfVar, SimpleTree, Maybe SimpleTree)]
    , stExtra :: SimpleExtra
    }
    deriving Show
instance Monoid SimpleTree where
    mempty = SimpleTree mempty mempty mempty
    mappend (SimpleTree a b c) (SimpleTree x y z) = SimpleTree
        (unionWith intersectVersionRanges a x)
        (b ++ y)
        (c ++ z)

data SimpleExtra = SimpleExtra
    { seTools :: Map PackageName VersionRange
    }
    deriving Show
instance Monoid SimpleExtra where
    mempty = SimpleExtra mempty
    mappend (SimpleExtra a) (SimpleExtra x) = SimpleExtra
        (unionWith intersectVersionRanges a x)

getSimpleTrees :: Bool -- ^ include test suites?
               -> Bool -- ^ include benchmarks?
               -> GenericPackageDescription
               -> [SimpleTree]
getSimpleTrees includeTests includeBench gpd = concat
    [ maybe [] (return . go libBuildInfo) $ condLibrary gpd
    , map (go buildInfo . snd) $ condExecutables gpd
    , if includeTests
        then map (go testBuildInfo . snd) $ condTestSuites gpd
        else []
    , if includeBench
        then map (go benchmarkBuildInfo . snd) $ condBenchmarks gpd
        else []
    ]
  where
    go getExtra (CondNode dat deps comps) = SimpleTree
        { stDeps = unionsWith intersectVersionRanges
                 $ map (\(Dependency x y) -> singletonMap x y) deps
        , stConds = map (goComp getExtra) comps
        , stExtra = toSimpleExtra $ getExtra dat
        }

    goComp getExtra (cond, tree1, mtree2) =
        (cond, go getExtra tree1, go getExtra <$> mtree2)

    toSimpleExtra bi = SimpleExtra
        { seTools = unionsWith intersectVersionRanges $ flip map (buildTools bi)
            $ \(Dependency name range) -> singletonMap name range
        }

data FlatComponent = FlatComponent
    { fcDeps :: Map PackageName VersionRange
    , fcExtra :: SimpleExtra
    }
    deriving Show
instance Monoid FlatComponent where
    mempty = FlatComponent mempty mempty
    mappend (FlatComponent a b) (FlatComponent x y) = FlatComponent
        (unionWith intersectVersionRanges a x)
        (b ++ y)

flattenComponent :: SimpleTree -> FlatComponent
flattenComponent (SimpleTree deps conds extra) =
    mconcat $ here : map goCond conds
  where
    here = FlatComponent { fcDeps = deps, fcExtra = extra }
    goCond (cond, tree1, mtree2)
        | checkCond cond = flattenComponent tree1
        | otherwise = maybe mempty flattenComponent mtree2

checkCond :: Condition ConfVar -> Bool
checkCond _ = False -- FIXME

simplifyDesc :: GenericPackageDescription -> IO (PackageBuild FlatComponent)
simplifyDesc gpd = do
    return PackageBuild
        { pbVersion = version
        , pbMaintainer = fmap snd $ lookup name $ pcPackages defaultPackageConstraints
        , pbGithubPings = getGithubPings gpd
        , pbUsers = mempty -- must be filled in later
        , pbFlags = packageFlags name
        , pbTestState =
            case () of
                ()
                    | not $ tryBuildTest name -> Don'tBuild
                    | name `member` pcExpectedFailures defaultPackageConstraints
                        -> ExpectFailure
                    | otherwise -> ExpectSuccess
        , pbHaddockState =
            case () of
                ()
                    | name `member` pcExpectedFailures defaultPackageConstraints
                        -> ExpectFailure
                    | otherwise -> ExpectSuccess
        , pbTryBuildBenchmark = tryBuildBenchmark name
        , pbDesc = foldMap flattenComponent $ getSimpleTrees
            (tryBuildTest name)
            (tryBuildBenchmark name)
            gpd
        }
  where
    PackageIdentifier name version = package $ packageDescription gpd

isAllowed :: Map PackageName Version -- ^ core
          -> PackageName -> Version -> Bool
isAllowed core = \name version ->
    case lookup name core of
        Just _ -> False -- never reinstall a core package
        Nothing ->
            case lookup name $ pcPackages defaultPackageConstraints of
                Nothing -> True -- no constraints
                Just (range, _) -> withinRange version range
