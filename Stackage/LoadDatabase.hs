module Stackage.LoadDatabase where

import qualified Codec.Archive.Tar                     as Tar
import           Control.Exception                     (throwIO)
import qualified Data.ByteString.Lazy                  as L
import qualified Data.ByteString.Lazy.Char8            as L8
import qualified Data.Map                              as Map
import           Data.Monoid                           (Monoid (..))
import           Data.Set                              (member)
import qualified Data.Set                              as Set
import           Distribution.Package                  (Dependency (Dependency))
import           Distribution.PackageDescription       (condExecutables,
                                                        condLibrary,
                                                        condTestSuites,
                                                        condTreeConstraints)
import           Distribution.PackageDescription.Parse (ParseResult (ParseOk),
                                                        parsePackageDescription)
import           Distribution.Version                  (withinRange)
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
loadPackageDB :: Set PackageName -- ^ core packages
              -> Map PackageName VersionRange -- ^ additional deps
              -> IO PackageDB
loadPackageDB core deps = do
    tarName <- getTarballName
    lbs <- L.readFile tarName
    addEntries mempty $ Tar.read lbs
  where
    addEntries :: PackageDB -> Tar.Entries Tar.FormatError -> IO PackageDB
    addEntries _ (Tar.Fail e) = throwIO e
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
                        Just vrange
                            | not $ withinRange v vrange -> return pdb
                        _ ->
                            case Tar.entryContent e of
                                Tar.NormalFile bs _ -> return $ mappend pdb $ PackageDB $ Map.singleton p PackageInfo
                                    { piVersion = v
                                    , piDeps = parseDeps bs
                                    }
                                _ -> return pdb

    parseDeps lbs =
        case parsePackageDescription $ L8.unpack lbs of
            ParseOk _ gpd -> mconcat
                [ maybe mempty go $ condLibrary gpd
                , mconcat $ map (go . snd) $ condExecutables gpd
                , mconcat $ map (go . snd) $ condTestSuites gpd
                -- , mconcat $ map (go . snd) $ condBenchmarks gpd
                ]
            _ -> mempty
      where
        go = Set.fromList . map (\(Dependency p _) -> p) . condTreeConstraints
