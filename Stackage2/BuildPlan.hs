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
import Stackage2.BuildConstraints
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
    { bpSystemInfo :: SystemInfo
    , bpTools       :: Vector (PackageName, Version)
    , bpPackages :: Map PackageName (PackageBuild desc)
    }
    deriving (Functor, Foldable, Traversable, Show, Eq)
type instance Element (BuildPlan desc) = desc
instance MonoFunctor (BuildPlan desc)
instance MonoFoldable (BuildPlan desc)
instance MonoTraversable (BuildPlan desc)

instance ToJSON (BuildPlan desc) where
    toJSON BuildPlan {..} = object
        [ "system-info" .= bpSystemInfo
        , "tools" .= map goTool bpTools
        , "packages" .= Map.mapKeysWith const unPackageName bpPackages
        ]
      where
        goTool (k, v) = object
            [ "name" .= display k
            , "version" .= display v
            ]
instance desc ~ () => FromJSON (BuildPlan desc) where
    parseJSON = withObject "BuildPlan" $ \o -> do
        bpSystemInfo <- o .: "system-info"
        bpTools <- (o .: "tools") >>= mapM goTool
        bpPackages <- Map.mapKeysWith const mkPackageName <$> (o .: "packages")
        return BuildPlan {..}
      where
        goTool = withObject "Tool" $ \o -> (,)
            <$> ((o .: "name") >>=
                either (fail . show) return . simpleParse . asText)
            <*> ((o .: "version") >>=
                either (fail . show) return . simpleParse . asText)

data PackageBuild desc = PackageBuild
    { pbVersion           :: Version
    , pbGithubPings       :: Set Text
    , pbUsers             :: Set PackageName
    , pbPackageConstraints :: PackageConstraints
    , pbDesc              :: desc
    }
    deriving (Functor, Foldable, Traversable, Show, Eq)
type instance Element (PackageBuild desc) = desc
instance MonoFunctor (PackageBuild desc)
instance MonoFoldable (PackageBuild desc)
instance MonoTraversable (PackageBuild desc)

instance ToJSON (PackageBuild desc) where
    toJSON PackageBuild {..} = object
        [ "version" .= asText (display pbVersion)
        , "github-pings" .= pbGithubPings
        , "users" .= map unPackageName (unpack pbUsers)
        , "constraints" .= pbPackageConstraints
        ]
instance desc ~ () => FromJSON (PackageBuild desc) where
    parseJSON = withObject "PackageBuild" $ \o -> do
        pbVersion <- o .: "version" >>= efail . simpleParse . asText
        pbGithubPings <- o .:? "github-pings" .!= mempty
        pbUsers <- Set.map PackageName <$> (o .:? "users" .!= mempty)
        pbPackageConstraints <- o .: "constraints"
        return PackageBuild {..}
      where
        pbDesc = ()
        efail = either (fail . show) return

newBuildPlan :: MonadIO m => BuildConstraints -> m (BuildPlan FlatComponent)
newBuildPlan bc@BuildConstraints {..} = liftIO $ do
    extraOrig <- getLatestDescriptions (isAllowed bc) (mkPackageBuild bc)
    let toolMap = makeToolMap extraOrig
        extra = populateUsers $ removeUnincluded bc toolMap extraOrig
        toolNames :: [ExeName]
        toolNames = concatMap (Map.keys . seTools . fcExtra . pbDesc) extra
    tools <- topologicalSortTools toolMap $ mapFromList $ do
        exeName <- toolNames
        guard $ exeName `notMember` siCoreExecutables
        packageName <- maybe mempty setToList $ lookup exeName toolMap
        packageBuild <- maybeToList $ lookup packageName extraOrig
        return (packageName, packageBuild)
    -- FIXME topologically sort packages? maybe just leave that to the build phase
    return BuildPlan
        { bpSystemInfo = bcSystemInfo
        , bpTools = tools
        , bpPackages = extra
        }
  where
    SystemInfo {..} = bcSystemInfo

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

-- | Include only packages which are dependencies of the required packages and
-- their build tools.
removeUnincluded :: BuildConstraints
                 -> Map ExeName (Set PackageName)
                 -> Map PackageName (PackageBuild FlatComponent)
                 -> Map PackageName (PackageBuild FlatComponent)
removeUnincluded BuildConstraints {..} toolMap orig =
    mapFromList $ filter (\(x, _) -> x `member` included) $ mapToList orig
  where
    SystemInfo {..} = bcSystemInfo

    included :: Set PackageName
    included = flip execState mempty $ mapM_ add bcPackages

    add name = do
        inc <- get
        when (name `notMember` inc) $ do
            put $ insertSet name inc
            case lookup name orig of
                Nothing -> return ()
                Just pb -> do
                    mapM_ (add . fst) $ mapToList $ fcDeps $ pbDesc pb
                    forM_ (map fst $ mapToList $ seTools $ fcExtra $ pbDesc pb) $
                        \exeName -> when (exeName `notMember` siCoreExecutables)
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

-- | Check whether the given package/version combo meets the constraints
-- currently in place.
isAllowed :: BuildConstraints
          -> PackageName -> Version -> Bool
isAllowed bc = \name version ->
    case lookup name $ siCorePackages $ bcSystemInfo bc of
        Just _ -> False -- never reinstall a core package
        Nothing -> withinRange version $ pcVersionRange $ bcPackageConstraints bc name

mkPackageBuild :: MonadThrow m
               => BuildConstraints
               -> GenericPackageDescription
               -> m (PackageBuild FlatComponent)
mkPackageBuild bc gpd = do
    pbDesc <- getFlattenedComponent CheckCond {..} gpd
    return PackageBuild {..}
  where
    PackageIdentifier name pbVersion = package $ packageDescription gpd
    pbGithubPings = getGithubPings gpd
    pbPackageConstraints = bcPackageConstraints bc name
    pbUsers = mempty -- must be filled in later

    ccPackageName = name
    ccOS = siOS
    ccArch = siArch
    ccCompilerFlavor = Distribution.Compiler.GHC
    ccCompilerVersion = siGhcVersion
    ccFlags = flags
    ccIncludeTests = pcTests pbPackageConstraints /= Don'tBuild
    ccIncludeBenchmarks = pcBuildBenchmarks pbPackageConstraints

    SystemInfo {..} = bcSystemInfo bc

    overrides = pcFlagOverrides pbPackageConstraints
    getFlag MkFlag {..} =
        (flagName, fromMaybe flagDefault $ lookup flagName overrides)
    flags = mapFromList $ map getFlag $ genPackageFlags gpd
