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
import Distribution.Version            (withinRange, anyVersion, simplifyVersionRange)
import Stackage2.PackageConstraints
import Stackage2.PackageIndex
import Stackage2.Prelude
import Stackage2.GithubPings
import Control.Monad.State.Strict (execState, get, put)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Aeson
import Stackage2.PackageDescription
import qualified Distribution.System
import qualified Distribution.Compiler

data BuildPlan desc = BuildPlan
    { bpCore        :: Map PackageName Version
    , bpCoreExecutables :: Set ExeName
    , bpGhcVersion :: Version
    , bpOS :: Distribution.System.OS
    , bpArch :: Distribution.System.Arch
    , bpTools       :: Vector (PackageName, Version)
    , bpExtra       :: Map PackageName (PackageBuild desc)
    }
    deriving (Functor, Foldable, Traversable, Show, Eq)
type instance Element (BuildPlan desc) = desc
instance MonoFunctor (BuildPlan desc)
instance MonoFoldable (BuildPlan desc)
instance MonoTraversable (BuildPlan desc)

instance ToJSON (BuildPlan desc) where
    toJSON BuildPlan {..} = object
        [ "core" .= asMap (mapFromList $ map toCore $ mapToList bpCore)
        , "core-exes" .= bpCoreExecutables
        , "ghc-version" .= asText (display bpGhcVersion)
        , "os" .= asText (display bpOS)
        , "arch" .= asText (display bpArch)
        , "tools" .= map goTool bpTools
        , "extra" .= Map.mapKeysWith const (unPackageName) bpExtra
        ]
      where
        toCore (x, y) = (asText $ display x, asText $ display y)
        goTool (name, version) = object
            [ "name" .= asText (display name)
            , "version" .= asText (display version)
            ]
instance desc ~ () => FromJSON (BuildPlan desc) where
    parseJSON = withObject "BuildPlan" $ \o -> do
        bpCore <- (o .: "core") >>= goCore
        bpCoreExecutables <- o .: "core-exes"
        bpGhcVersion <- (o .: "ghc-version") >>= either (fail . show) return . simpleParse . asText
        bpOS <- o .: "os" >>= either (fail . show) return . simpleParse . asText
        bpArch <- (o .: "arch") >>= either (fail . show) return . simpleParse . asText
        bpTools <- (o .: "tools") >>= mapM goTool
        bpExtra <- goExtra <$> (o .: "extra")
        return BuildPlan {..}
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

data PackageBuild desc = PackageBuild
    { pbVersion           :: Version
    , pbVersionRange      :: VersionRange
    -- ^ This is vital for ensuring old constraints are kept in place when bumping
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

-- | There seems to be a bug in Cabal where serializing and deserializing
-- version ranges winds up with different representations. So we have a
-- super-simplifier to deal with that.
superSimplifyVersionRange :: VersionRange -> VersionRange
superSimplifyVersionRange vr =
    fromMaybe (assert False vr') $ simpleParse $ asList $ display vr'
  where
    vr' = simplifyVersionRange vr

instance ToJSON (PackageBuild desc) where
    toJSON PackageBuild {..} = object $ concat
        [ maybe [] (\m -> ["maintainer" .= m]) pbMaintainer
        ,
            [ "version" .= asText (display pbVersion)
            , "version-range" .= asText (display $ superSimplifyVersionRange pbVersionRange)
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
        <*> (o .: "version-range" >>= fmap superSimplifyVersionRange . efail . simpleParse . asText)
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

newBuildPlan :: MonadIO m => PackageConstraints -> m (BuildPlan FlatComponent)
newBuildPlan pc = liftIO $ do
    extraOrig <- getLatestDescriptions (isAllowed pc) (mkPackageBuild pc)
    let toolMap = makeToolMap extraOrig
        extra = populateUsers $ removeUnincluded pc toolMap extraOrig
        toolNames :: [ExeName]
        toolNames = concatMap (Map.keys . seTools . fcExtra . pbDesc) extra
    tools <- topologicalSortTools toolMap $ mapFromList $ do
        exeName <- toolNames
        guard $ exeName `notMember` pcCoreExecutables pc
        packageName <- maybe mempty setToList $ lookup exeName toolMap
        packageBuild <- maybeToList $ lookup packageName extraOrig
        return (packageName, packageBuild)
    -- FIXME topologically sort packages? maybe just leave that to the build phase
    return BuildPlan
        { bpCore = pcCorePackages pc
        , bpCoreExecutables = pcCoreExecutables pc
        , bpGhcVersion = pcGhcVersion pc
        , bpOS = pcOS pc
        , bpArch = pcArch pc
        , bpTools = tools
        , bpExtra = extra
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

removeUnincluded :: PackageConstraints
                 -> Map ExeName (Set PackageName)
                 -> Map PackageName (PackageBuild FlatComponent)
                 -> Map PackageName (PackageBuild FlatComponent)
removeUnincluded pc toolMap orig =
    mapFromList $ filter (\(x, _) -> x `member` included) $ mapToList orig
  where
    coreExes = pcCoreExecutables pc

    included :: Set PackageName
    included = flip execState mempty $
        mapM_ (add . fst) $ mapToList $ pcPackages pc

    add name = do
        inc <- get
        when (name `notMember` inc) $ do
            put $ insertSet name inc
            case lookup name orig of
                Nothing -> return ()
                Just pb -> do
                    mapM_ (add . fst) $ mapToList $ fcDeps $ pbDesc pb
                    forM_ (map fst $ mapToList $ seTools $ fcExtra $ pbDesc pb) $
                        \exeName -> when (exeName `notMember` coreExes)
                            $ mapM_ add $ fromMaybe mempty $ lookup exeName toolMap

populateUsers :: Map PackageName (PackageBuild FlatComponent)
              -> Map PackageName (PackageBuild FlatComponent)
populateUsers orig =
    mapWithKey go orig
  where
    go name pb = pb { pbUsers = foldMap (go2 name) (mapToList orig) }

    go2 dep (user, pb)
        | dep `member` fcDeps (pbDesc pb) = singletonSet user
        | otherwise = mempty

isAllowed :: PackageConstraints
          -> PackageName -> Version -> Bool
isAllowed pc = \name version ->
    case lookup name $ pcCorePackages pc of
        Just _ -> False -- never reinstall a core package
        Nothing ->
            case lookup name $ pcPackages pc of
                Nothing -> True -- no constraints
                Just (range, _) -> withinRange version range

mkPackageBuild :: MonadThrow m
               => PackageConstraints
               -> GenericPackageDescription
               -> m (PackageBuild FlatComponent)
mkPackageBuild pc gpd = do
    let overrides = pcFlagOverrides pc name
        getFlag MkFlag {..} =
            (flagName, fromMaybe flagDefault $ lookup flagName overrides)
        flags = mapFromList $ map getFlag $ genPackageFlags gpd
    desc <- getFlattenedComponent
        CheckCond
            { ccPackageName = name
            , ccOS = pcOS pc
            , ccArch = pcArch pc
            , ccCompilerFlavor = Distribution.Compiler.GHC
            , ccCompilerVersion = pcGhcVersion pc
            , ccFlags = flags
            }
        (pcTests pc name /= Don'tBuild)
        (pcBuildBenchmark pc name)
        gpd
    return PackageBuild
        { pbVersion = version
        , pbVersionRange = superSimplifyVersionRange
                         $ maybe anyVersion fst $ lookup name $ pcPackages pc
        , pbMaintainer = lookup name (pcPackages pc) >>= snd
        , pbGithubPings = getGithubPings gpd
        , pbUsers = mempty -- must be filled in later
        , pbFlags = flags
        , pbTestState = pcTests pc name
        , pbHaddockState = pcHaddocks pc name
        , pbTryBuildBenchmark = pcBuildBenchmark pc name
        , pbDesc = desc
        }
  where
    PackageIdentifier name version = package $ packageDescription gpd
