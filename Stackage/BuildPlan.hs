{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeFamilies       #-}
-- | Representation of a concrete build plan, and how to generate a new one
-- based on constraints.
module Stackage.BuildPlan
    ( BuildPlan (..)
    , PackagePlan (..)
    , newBuildPlan
    , makeToolMap
    ) where

import           Control.Monad.State.Strict      (execState, get, put)
import           Data.Aeson
import qualified Data.Map                        as Map
import qualified Data.Set                        as Set
import qualified Distribution.Compiler
import           Distribution.PackageDescription
import           Stackage.BuildConstraints
import           Stackage.GithubPings
import           Stackage.PackageDescription
import           Stackage.PackageIndex
import           Stackage.Prelude

data BuildPlan = BuildPlan
    { bpSystemInfo  :: SystemInfo
    , bpTools       :: Vector (PackageName, Version)
    , bpPackages    :: Map PackageName PackagePlan
    , bpGithubUsers :: Map Text (Set Text)
    }
    deriving (Show, Eq)

instance ToJSON BuildPlan where
    toJSON BuildPlan {..} = object
        [ "system-info" .= bpSystemInfo
        , "tools" .= map goTool bpTools
        , "packages" .= Map.mapKeysWith const unPackageName bpPackages
        , "github-users" .= bpGithubUsers
        ]
      where
        goTool (k, v) = object
            [ "name" .= display k
            , "version" .= display v
            ]
instance FromJSON BuildPlan where
    parseJSON = withObject "BuildPlan" $ \o -> do
        bpSystemInfo <- o .: "system-info"
        bpTools <- (o .: "tools") >>= mapM goTool
        bpPackages <- Map.mapKeysWith const mkPackageName <$> (o .: "packages")
        bpGithubUsers <- o .:? "github-users" .!= mempty
        return BuildPlan {..}
      where
        goTool = withObject "Tool" $ \o -> (,)
            <$> ((o .: "name") >>=
                either (fail . show) return . simpleParse . asText)
            <*> ((o .: "version") >>=
                either (fail . show) return . simpleParse . asText)

data PackagePlan = PackagePlan
    { ppVersion     :: Version
    , ppGithubPings :: Set Text
    , ppUsers       :: Set PackageName
    , ppConstraints :: PackageConstraints
    , ppDesc        :: SimpleDesc
    }
    deriving (Show, Eq)

instance ToJSON PackagePlan where
    toJSON PackagePlan {..} = object
        [ "version"      .= asText (display ppVersion)
        , "github-pings" .= ppGithubPings
        , "users"        .= map unPackageName (unpack ppUsers)
        , "constraints"  .= ppConstraints
        , "description"  .= ppDesc
        ]
instance FromJSON PackagePlan where
    parseJSON = withObject "PackageBuild" $ \o -> do
        ppVersion <- o .: "version"
                 >>= either (fail . show) return
                   . simpleParse . asText
        ppGithubPings <- o .:? "github-pings" .!= mempty
        ppUsers <- Set.map PackageName <$> (o .:? "users" .!= mempty)
        ppConstraints <- o .: "constraints"
        ppDesc <- o .: "description"
        return PackagePlan {..}

newBuildPlan :: MonadIO m => BuildConstraints -> m BuildPlan
newBuildPlan bc@BuildConstraints {..} = liftIO $ do
    packagesOrig <- getLatestDescriptions (isAllowed bc) (mkPackagePlan bc)
    let toolMap = makeToolMap packagesOrig
        packages = populateUsers $ removeUnincluded bc toolMap packagesOrig
        toolNames :: [ExeName]
        toolNames = concatMap (Map.keys . sdTools . ppDesc) packages
    tools <- topologicalSortTools toolMap $ mapFromList $ do
        exeName <- toolNames
        guard $ exeName `notMember` siCoreExecutables
        packageName <- maybe mempty setToList $ lookup exeName toolMap
        packagePlan <- maybeToList $ lookup packageName packagesOrig
        return (packageName, packagePlan)
    -- FIXME topologically sort packages? maybe just leave that to the build phase
    return BuildPlan
        { bpSystemInfo = bcSystemInfo
        , bpTools = tools
        , bpPackages = packages
        , bpGithubUsers = bcGithubUsers
        }
  where
    SystemInfo {..} = bcSystemInfo

makeToolMap :: Map PackageName PackagePlan
            -> Map ExeName (Set PackageName)
makeToolMap =
    unionsWith (++) . map go . mapToList
  where
    go (packageName, pp) =
        foldMap go' $ sdProvidedExes $ ppDesc pp
      where
        go' exeName = singletonMap exeName (singletonSet packageName)

topologicalSortTools :: MonadThrow m
                     => Map ExeName (Set PackageName)
                     -> Map PackageName PackagePlan
                     -> m (Vector (PackageName, Version))
topologicalSortTools toolMap = topologicalSort
    ppVersion
    (concatMap (fromMaybe mempty . flip lookup toolMap) . Map.keys . sdTools . ppDesc)

-- | Include only packages which are dependencies of the required packages and
-- their build tools.
removeUnincluded :: BuildConstraints
                 -> Map ExeName (Set PackageName)
                 -> Map PackageName PackagePlan
                 -> Map PackageName PackagePlan
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
                    mapM_ add $ Map.keys $ sdPackages $ ppDesc pb
                    forM_ (Map.keys $ sdTools $ ppDesc pb) $
                        \exeName -> when (exeName `notMember` siCoreExecutables)
                            $ mapM_ add $ fromMaybe mempty $ lookup exeName toolMap

populateUsers :: Map PackageName PackagePlan
              -> Map PackageName PackagePlan
populateUsers orig =
    mapWithKey go orig
  where
    go name pb = pb { ppUsers = foldMap (go2 name) (mapToList orig) }

    go2 dep (user, pb)
        | dep `member` sdPackages (ppDesc pb) = singletonSet user
        | otherwise = mempty

-- | Check whether the given package/version combo meets the constraints
-- currently in place.
isAllowed :: BuildConstraints
          -> PackageName -> Version -> Bool
isAllowed bc = \name version ->
    case lookup name $ siCorePackages $ bcSystemInfo bc of
        Just _ -> False -- never reinstall a core package
        Nothing -> withinRange version $ pcVersionRange $ bcPackageConstraints bc name

mkPackagePlan :: MonadThrow m
              => BuildConstraints
              -> GenericPackageDescription
              -> m PackagePlan
mkPackagePlan bc gpd = do
    ppDesc <- toSimpleDesc CheckCond {..} gpd
    return PackagePlan {..}
  where
    PackageIdentifier name ppVersion = package $ packageDescription gpd
    ppGithubPings = getGithubPings bc gpd
    ppConstraints = bcPackageConstraints bc name
    ppUsers = mempty -- must be filled in later

    ccPackageName = name
    ccOS = siOS
    ccArch = siArch
    ccCompilerFlavor = Distribution.Compiler.GHC
    ccCompilerVersion = siGhcVersion
    ccFlags = flags
    ccIncludeTests = pcTests ppConstraints /= Don'tBuild
    ccIncludeBenchmarks = pcBuildBenchmarks ppConstraints

    SystemInfo {..} = bcSystemInfo bc

    overrides = pcFlagOverrides ppConstraints
    getFlag MkFlag {..} =
        (flagName, fromMaybe flagDefault $ lookup flagName overrides)
    flags = mapFromList $ map getFlag $ genPackageFlags gpd
