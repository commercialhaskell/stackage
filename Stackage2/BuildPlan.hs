{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
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
import Stackage2.PackageDescription

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
    coreExes <- getCoreExecutables
    extraOrig <- getLatestDescriptions (isAllowed core) mkPackageBuild
    let toolMap = makeToolMap extraOrig
        extra = populateUsers $ removeUnincluded toolMap extraOrig
        toolNames :: [ExeName]
        toolNames = concatMap (Map.keys . seTools . fcExtra . pbDesc) extra
    tools <- topologicalSortTools toolMap $ mapFromList $ do
        exeName <- toolNames
        guard $ exeName `notMember` coreExes
        packageName <- maybe mempty setToList $ lookup exeName toolMap
        packageBuild <- maybeToList $ lookup packageName extraOrig
        return (packageName, packageBuild)
    return BuildPlan
        { bpCore = core
        , bpTools = tools
        , bpExtra = extra
        , bpGlobalFlags = defaultGlobalFlags
        }

makeToolMap :: Map PackageName (PackageBuild FlatComponent)
            -> Map ExeName (Set PackageName)
makeToolMap =
    unionsWith (++) . map go . mapToList
  where
    go (packageName, pb) =
        foldMap go' $ seProvidedExes $ fcExtra $ pbDesc pb
      where
        go' exeName = singletonMap exeName (singletonSet packageName)

topologicalSortTools :: MonadThrow m
                     => Map ExeName (Set PackageName)
                     -> Map PackageName (PackageBuild FlatComponent)
                     -> m (Vector (PackageName, Version))
topologicalSortTools toolMap = topologicalSort
    pbVersion
    (concatMap (fromMaybe mempty . flip lookup toolMap) . Map.keys . seTools . fcExtra . pbDesc)

topologicalSort :: (Ord key, Show key, MonadThrow m, Typeable key)
                => (value -> finalValue)
                -> (value -> Set key) -- ^ deps
                -> Map key value
                -> m (Vector (key, finalValue))
topologicalSort toFinal toDeps =
    loop id . mapWithKey removeSelfDeps . fmap (toDeps &&& toFinal)
  where
    removeSelfDeps k (deps, final) = (deleteSet k deps, final)
    loop front toProcess | null toProcess = return $ pack $ front []
    loop front toProcess
        | null noDeps = throwM $ NoEmptyDeps (map fst toProcess')
        | otherwise = loop (front . noDeps') (mapFromList hasDeps)
      where
        toProcess' = fmap (first removeUnavailable) toProcess
        allKeys = Map.keysSet toProcess
        removeUnavailable = asSet . setFromList . filter (`member` allKeys) . setToList
        (noDeps, hasDeps) = partition (null . fst . snd) $ mapToList toProcess'
        noDeps' = (map (second snd) noDeps ++)

data TopologicalSortException key = NoEmptyDeps (Map key (Set key))
    deriving (Show, Typeable)
instance (Show key, Typeable key) => Exception (TopologicalSortException key)

removeUnincluded :: Map ExeName (Set PackageName)
                 -> Map PackageName (PackageBuild FlatComponent)
                 -> Map PackageName (PackageBuild FlatComponent)
removeUnincluded toolMap orig =
    mapFromList $ filter (\(x, _) -> x `member` included) $ mapToList orig
  where
    included :: Set PackageName
    included = flip execState mempty $
        mapM_ (add . fst) $ mapToList $ pcPackages defaultPackageConstraints

    add name = do
        inc <- get
        when (name `notMember` inc) $ do
            put $ insertSet name inc
            case lookup name orig of
                Nothing -> return ()
                Just pb -> do
                    mapM_ (add . fst) $ mapToList $ fcDeps $ pbDesc pb
                    forM_ (map fst $ mapToList $ seTools $ fcExtra $ pbDesc pb) $
                        \exeName -> mapM_ add $ fromMaybe mempty $ lookup exeName toolMap

populateUsers :: Map PackageName (PackageBuild FlatComponent)
              -> Map PackageName (PackageBuild FlatComponent)
populateUsers orig =
    mapWithKey go orig
  where
    go name pb = pb { pbUsers = foldMap (go2 name) (mapToList orig) }

    go2 dep (user, pb)
        | dep `member` fcDeps (pbDesc pb) = singletonSet user
        | otherwise = mempty

isAllowed :: Map PackageName Version -- ^ core
          -> PackageName -> Version -> Bool
isAllowed core = \name version ->
    case lookup name core of
        Just _ -> False -- never reinstall a core package
        Nothing ->
            case lookup name $ pcPackages defaultPackageConstraints of
                Nothing -> True -- no constraints
                Just (range, _) -> withinRange version range

mkPackageBuild :: Monad m
               => GenericPackageDescription
               -> m (PackageBuild FlatComponent)
mkPackageBuild gpd =
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
        , pbDesc = getFlattenedComponent
            (tryBuildTest name)
            (tryBuildBenchmark name)
            gpd
        }
  where
    PackageIdentifier name version = package $ packageDescription gpd
