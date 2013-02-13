module Stackage.LoadDatabase where

import qualified Codec.Archive.Tar                     as Tar
import           Control.Monad                         (guard)
import qualified Data.ByteString.Lazy                  as L
import qualified Data.ByteString.Lazy.Char8            as L8
import           Data.List                             (stripPrefix)
import qualified Data.Map                              as Map
import           Data.Maybe                            (catMaybes, listToMaybe,
                                                        mapMaybe)
import           Data.Monoid                           (Monoid (..))
import           Data.Set                              (member)
import qualified Data.Set                              as Set
import           Distribution.Compiler                 (CompilerFlavor (GHC))
import           Distribution.Package                  (Dependency (Dependency))
import           Distribution.PackageDescription       (Condition (..),
                                                        ConfVar (..),
                                                        FlagName (FlagName),
                                                        RepoType (Git),
                                                        SourceRepo (..),
                                                        benchmarkBuildInfo,
                                                        buildInfo, buildTools,
                                                        condBenchmarks,
                                                        condExecutables,
                                                        condLibrary,
                                                        condTestSuites,
                                                        condTreeComponents,
                                                        condTreeConstraints,
                                                        condTreeData,
                                                        flagDefault, flagName,
                                                        genPackageFlags,
                                                        homepage, libBuildInfo,
                                                        packageDescription,
                                                        sourceRepos,
                                                        testBuildInfo)
import           Distribution.PackageDescription.Parse (ParseResult (ParseOk),
                                                        parsePackageDescription)
import           Distribution.System                   (buildArch, buildOS)
import           Distribution.Version                  (unionVersionRanges,
                                                        withinRange)
import           Stackage.Config
import           Stackage.Types
import           Stackage.Util

-- | Load the raw package database.
--
-- We want to put in some restrictions:
--
-- * Drop all core packages. We never want to install a new version of
-- those, nor include them in the package list.
--
-- * For packages with a specific version bound, find the maximum matching
-- version.
--
-- * For other packages, select the maximum version number.
loadPackageDB :: SelectSettings
              -> Set PackageName -- ^ core packages
              -> Map PackageName (VersionRange, Maintainer) -- ^ additional deps
              -> IO PackageDB
loadPackageDB settings core deps = do
    tarName <- getTarballName
    lbs <- L.readFile tarName
    addEntries mempty $ Tar.read lbs
  where
    addEntries _ (Tar.Fail e) = error $ show e
    addEntries db Tar.Done = return db
    addEntries db (Tar.Next e es) = addEntry db e >>= flip addEntries es

    addEntry :: PackageDB -> Tar.Entry -> IO PackageDB
    addEntry pdb e =
        case getPackageVersion e of
            Nothing -> return pdb
            Just (p, v)
                | p `member` core -> return pdb
                | otherwise ->
                    case Map.lookup p deps of
                        Just (vrange, _maintainer)
                            | not $ withinRange v vrange -> return pdb
                        _ ->
                            case Tar.entryContent e of
                                Tar.NormalFile bs _ -> do
                                    let (deps', hasTests, buildTools', mgpd, execs, mgithub) = parseDeps bs
                                    return $ mappend pdb $ PackageDB $ Map.singleton p PackageInfo
                                        { piVersion = v
                                        , piDeps = deps'
                                        , piHasTests = hasTests
                                        , piBuildTools = buildTools'
                                        , piGPD = mgpd
                                        , piExecs = execs
                                        , piGithubUser = mgithub
                                        }
                                _ -> return pdb

    parseDeps lbs =
        case parsePackageDescription $ L8.unpack lbs of
            ParseOk _ gpd -> (mconcat
                [ maybe mempty (go gpd) $ condLibrary gpd
                , mconcat $ map (go gpd . snd) $ condExecutables gpd
                , mconcat $ map (go gpd . snd) $ condTestSuites gpd
                , mconcat $ map (go gpd . snd) $ condBenchmarks gpd
                ], not $ null $ condTestSuites gpd
                , Set.fromList $ map depName $ allBuildInfo gpd
                , Just gpd
                , Set.fromList $ map (Executable . fst) $ condExecutables gpd
                , listToMaybe $ catMaybes
                  $ parseGithubUserHP (homepage $ packageDescription gpd)
                  : map parseGithubUserSR (sourceRepos $ packageDescription gpd)
                )
            _ -> (mempty, defaultHasTestSuites, Set.empty, Nothing, Set.empty, Nothing)
      where
        allBuildInfo gpd = concat
            [ maybe mempty (goBI libBuildInfo) $ condLibrary gpd
            , concat $ map (goBI buildInfo . snd) $ condExecutables gpd
            , concat $ map (goBI testBuildInfo . snd) $ condTestSuites gpd
            , concat $ map (goBI benchmarkBuildInfo . snd) $ condBenchmarks gpd
            ]
          where
            goBI f x = buildTools $ f $ condTreeData x
        depName (Dependency (PackageName p) _) = Executable p
        go gpd tree
            = Map.unionsWith unionVersionRanges
            $ Map.fromList (map (\(Dependency p vr) -> (p, vr)) $ condTreeConstraints tree)
            : map (go gpd) (mapMaybe (checkCond gpd) $ condTreeComponents tree)

        checkCond gpd (cond, tree, melse)
            | checkCond' cond = Just tree
            | otherwise = melse
          where
            checkCond' (Var (OS os)) = os == buildOS
            checkCond' (Var (Arch arch)) = arch == buildArch
            checkCond' (Var (Flag flag@(FlagName flag'))) =
                flag' `Set.notMember` disabledFlags settings &&
                flag `elem` flags'
            checkCond' (Var (Impl compiler range)) =
                compiler == GHC && withinRange targetCompilerVersion range
            checkCond' (Lit b) = b
            checkCond' (CNot c) = not $ checkCond' c
            checkCond' (COr c1 c2) = checkCond' c1 || checkCond' c2
            checkCond' (CAnd c1 c2) = checkCond' c1 && checkCond' c2

            flags' = map flagName (filter flagDefault $ genPackageFlags gpd) ++
                     (map FlagName $ Set.toList $ Stackage.Types.flags settings)

-- | Attempt to grab the Github username from a homepage.
parseGithubUserHP :: String -> Maybe String
parseGithubUserHP url1 = do
    url2 <- listToMaybe $ mapMaybe (flip stripPrefix url1)
        [ "http://github.com/"
        , "https://github.com/"
        ]
    let x = takeWhile (/= '/') url2
    guard $ not $ null x
    Just x

-- | Attempt to grab the Github username from a source repo.
parseGithubUserSR :: SourceRepo -> Maybe String
parseGithubUserSR sr =
    case (repoType sr, repoLocation sr) of
        (Just Git, Just s) -> parseGithubUserHP s
        _ -> Nothing
