{-# LANGUAGE ScopedTypeVariables #-}
module Stackage.LoadDatabase where

import qualified Codec.Archive.Tar                     as Tar
import qualified Codec.Compression.GZip                as GZip
import           Control.Exception                     (IOException, handle)
import           Control.Monad                         (guard, foldM)
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
import           Distribution.Text                     (simpleParse)
import           Distribution.Version                  (Version (Version),
                                                        unionVersionRanges,
                                                        withinRange)
import           Stackage.Types
import           Stackage.Util
import           System.Directory                      (doesFileExist, getDirectoryContents)
import           System.FilePath                       ((<.>), (</>))

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
              -> Map PackageName Version -- ^ core packages from HP file
              -> Set PackageName -- ^ all core packages, including extras
              -> Map PackageName (VersionRange, Maintainer) -- ^ additional deps
              -> IO PackageDB
loadPackageDB settings coreMap core deps = do
    tarName <- getTarballName
    lbs <- L.readFile tarName
    pdb <- addEntries mempty $ Tar.read lbs
    contents <- handle (\(_ :: IOException) -> return [])
              $ getDirectoryContents $ selectTarballDir settings
    foldM addTarball pdb $ mapMaybe stripTarGz contents
  where
    addEntries _ (Tar.Fail e) = error $ show e
    addEntries db Tar.Done = return db
    addEntries db (Tar.Next e es) = addEntry db e >>= flip addEntries es

    stripTarGz = fmap reverse . stripPrefix (reverse ".tar.gz") . reverse

    ghcVersion' =
        let GhcMajorVersion x y = selectGhcVersion settings
         in Version [x, y, 2] []

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
                        _ -> do
                            let pkgname = packageVersionString (p, v)
                                tarball = selectTarballDir settings </> pkgname <.> "tar.gz"
                            exists <- doesFileExist tarball
                            if exists
                                then do
                                    lbs <- L.readFile tarball
                                    findCabalAndAddPackage tarball p v pdb $ Tar.read $ GZip.decompress lbs
                                else
                                    case Tar.entryContent e of
                                        Tar.NormalFile bs _ -> addPackage p v bs pdb
                                        _ -> return pdb

    addTarball :: PackageDB -> FilePath -> IO PackageDB
    addTarball pdb tarball' = do
        lbs <- L.readFile tarball
        let (v', p') = break (== '-') $ reverse tarball'
            p = PackageName $ reverse $ drop 1 p'
        v <- maybe (error $ "Invalid tarball name: " ++ tarball) return
           $ simpleParse $ reverse v'
        findCabalAndAddPackage tarball p v pdb $ Tar.read $ GZip.decompress lbs
      where
        tarball = selectTarballDir settings </> tarball' <.> "tar.gz"

    skipTests p = p `Set.member` skippedTests settings

    -- Find the relevant cabal file in the given entries and add its contents
    -- to the package database
    findCabalAndAddPackage tarball p v pdb =
        loop
      where
        fixPath '\\' = '/'
        fixPath c = c

        expectedPath = let PackageName p' = p in concat
            [ packageVersionString (p, v)
            , "/"
            , p'
            , ".cabal"
            ]
        loop Tar.Done = error $ concat
            [ "Missing cabal file "
            , show expectedPath
            , " in tarball: "
            , show tarball
            ]
        loop (Tar.Fail e) = error $ concat
            [ "Unable to read tarball "
            , show tarball
            , ": "
            , show e
            ]
        loop (Tar.Next entry rest)
            | map fixPath (Tar.entryPath entry) == expectedPath =
                case Tar.entryContent entry of
                    Tar.NormalFile bs _ -> addPackage p v bs pdb
                    _ -> error $ concat
                        [ "In tarball "
                        , show tarball
                        , " the cabal file "
                        , show expectedPath
                        , " was not a normal file"
                        ]
            | otherwise = loop rest

    addPackage p v lbs pdb = do
        let (deps', hasTests, buildToolsExe', buildToolsOther', mgpd, execs, mgithub) = parseDeps p lbs
        return $ mappend pdb $ PackageDB $ Map.singleton p PackageInfo
            { piVersion = v
            , piDeps = deps'
            , piHasTests = hasTests
            , piBuildToolsExe = buildToolsExe'
            , piBuildToolsAll = buildToolsExe' `Set.union` buildToolsOther'
            , piGPD = mgpd
            , piExecs = execs
            , piGithubUser = mgithub
            }

    parseDeps p lbs =
        case parsePackageDescription $ L8.unpack lbs of
            ParseOk _ gpd -> (mconcat
                [ maybe mempty (go gpd) $ condLibrary gpd
                , mconcat $ map (go gpd . snd) $ condExecutables gpd
                , if skipTests p
                    then mempty
                    else mconcat $ map (go gpd . snd) $ condTestSuites gpd
                -- FIXME , mconcat $ map (go gpd . snd) $ condBenchmarks gpd
                ], not $ null $ condTestSuites gpd
                , Set.fromList $ map depName $ libExeBuildInfo gpd
                , Set.fromList $ map depName $ testBenchBuildInfo gpd
                , Just gpd
                , Set.fromList $ map (Executable . fst) $ condExecutables gpd
                , listToMaybe $ catMaybes
                  $ parseGithubUserHP (homepage $ packageDescription gpd)
                  : map parseGithubUserSR (sourceRepos $ packageDescription gpd)
                )
            _ -> (mempty, defaultHasTestSuites, Set.empty, Set.empty, Nothing, Set.empty, Nothing)
      where
        libExeBuildInfo gpd = concat
            [ maybe mempty (goBI libBuildInfo) $ condLibrary gpd
            , concat $ map (goBI buildInfo . snd) $ condExecutables gpd
            ]
        testBenchBuildInfo gpd = concat
            [ if skipTests p
                then []
                else concat $ map (goBI testBuildInfo . snd) $ condTestSuites gpd
            , concat $ map (goBI benchmarkBuildInfo . snd) $ condBenchmarks gpd
            ]
        goBI f x = buildTools $ f $ condTreeData x

        depName (Dependency (PackageName pn) _) = Executable pn
        go gpd tree
            = Map.filterWithKey (\k _ -> not $ ignoredDep k)
            $ Map.unionsWith unionVersionRanges
            $ Map.fromList (map (\(Dependency pn vr) -> (pn, vr)) $ condTreeConstraints tree)
            : map (go gpd) (mapMaybe (checkCond gpd) $ condTreeComponents tree)

        -- Some specific overrides for cases where getting Stackage to be smart
        -- enough to handle things would be too difficult.
        ignoredDep :: PackageName -> Bool
        ignoredDep dep
            -- The flag logic used by text-stream-decode confuses Stackage.
            | dep == PackageName "text" && p == PackageName "text-stream-decode" = True
            | otherwise = False

        checkCond gpd (cond, tree, melse)
            | checkCond' cond = Just tree
            | otherwise = melse
          where
            checkCond' (Var (OS os)) = os == buildOS
            checkCond' (Var (Arch arch)) = arch == buildArch

            -- Sigh... the small_base flag on mersenne-random-pure64 is backwards
            checkCond' (Var (Flag (FlagName "small_base")))
                | p == PackageName "mersenne-random-pure64" = False

            checkCond' (Var (Flag flag@(FlagName flag'))) =
                flag' `Set.notMember` disabledFlags settings &&
                flag `elem` flags'
            checkCond' (Var (Impl compiler range)) =
                compiler == GHC && withinRange ghcVersion' range
            checkCond' (Lit b) = b
            checkCond' (CNot c) = not $ checkCond' c
            checkCond' (COr c1 c2) = checkCond' c1 || checkCond' c2
            checkCond' (CAnd c1 c2) = checkCond' c1 && checkCond' c2

            flags' = map flagName (filter flagDefault $ genPackageFlags gpd) ++
                     (map FlagName $ Set.toList $ Stackage.Types.flags settings coreMap)

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
