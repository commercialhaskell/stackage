{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Main where

import Control.Arrow
import Control.Monad
import Control.Monad.State
import Data.Aeson
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.String.Conversions
import Distribution.Text (display, simpleParse)
import Distribution.Types.VersionRange (VersionRange, normaliseVersionRange, anyVersion, intersectVersionRanges, majorBoundVersion, earlierVersion)
import GHC.Generics
import RIO ()
import RIO.Map (Map)
import System.IO
import qualified Data.Yaml as Y
import qualified Distribution.Types.PackageName as C (PackageName, mkPackageName)
import qualified Distribution.Types.Version as C (Version, mkVersion)
import qualified RIO.Map as M

src :: String
src = "../../build-constraints.yaml"

target :: String
target = "../../lts-build-constraints.yaml"

newtype PackageName = PackageName { unPackageName :: C.PackageName }
  deriving (Eq, Generic, Ord, FromJSONKey, Show)

instance FromJSON PackageName where
  parseJSON = fmap (PackageName . C.mkPackageName) . parseJSON

newtype Version = Version { unVersion :: C.Version }
  deriving (Generic, Show)

instance FromJSON Version where
  parseJSON v = do
    s <- parseJSON @ String v
    case simpleParse s of
      Nothing -> fail "Invalid Version"
      Just v -> pure $ Version v


data PackageDecl = PackageDecl
  { prefix :: String
  , package :: PackageName
  , range :: VersionRange
  , suffix :: String
  }

takeDropWhile :: (Char -> Bool) -> String -> Maybe (String, String)
takeDropWhile p s = if null a then Nothing else Just (a, b)
  where
    (a, b) = takeDropWhile_ p s

takeDropWhile_ :: (Char -> Bool) -> String -> (String, String)
takeDropWhile_ p s = (takeWhile p s, dropWhile p s)

takePrefix :: String -> String -> Maybe (String, String)
takePrefix p s =
  if p `isPrefixOf` s
  then Just (p, drop (length p) s)
  else Nothing

takePackageName :: String -> Maybe (PackageName, String)
takePackageName = fmap (first (PackageName . C.mkPackageName)) . takeDropWhile (/= ' ')

maybeTakeVersionRange :: String -> (Maybe VersionRange, String)
maybeTakeVersionRange s = (simpleParse range, comment)
  where
    (range, comment) = takeDropWhile_ (/= '#') s

p_packageDecl :: String -> Maybe PackageDecl
p_packageDecl s = do
  (prefix, s') <- takePrefix "        - " s
  (package, s'') <- takePackageName s'
  let (range, s''') = maybeTakeVersionRange s''
  pure PackageDecl { prefix, package, range = fromMaybe anyVersion range, suffix = s''' }

handlePackage :: Map PackageName Version -> PackageDecl -> String
handlePackage snap PackageDecl { prefix, package, range, suffix } =
  prefix ++ display (unPackageName package) ++ rng ++ suff
  where
    suff = if null suffix then suffix else (' ': suffix)


    rng = case intersect (majorBoundVersion . unVersion <$> snapshotVersion) range of
      Just rngI | rngI == anyVersion -> ""
      Nothing -> ""
      Just rngI -> (' ' :) . (\(a,b) -> a <> " " <> b) . takeDropWhile_ (not . isDigit) $ display rngI
    snapshotVersion = M.lookup package snap

    intersect Nothing _ = Just . earlierVersion $ C.mkVersion [0] -- package not in snapshot
    intersect (Just a) b =
      if b == anyVersion -- drop `&& -any`
      then Just a
      else Just $ normaliseVersionRange (intersectVersionRanges a b)


data Snapshot = Snapshot
  { packages :: [SnapshotPackage]
  } deriving (FromJSON, Generic, Show)

data SnapshotPackage = SnapshotPackage
  { hackage :: PackageVersion
  } deriving (FromJSON, Generic, Show)

data PackageVersion = PackageVersion
  { pvPackage :: PackageName
  , pvVersion :: Version
  } deriving Show

instance FromJSON PackageVersion where
  parseJSON s0 = do
    s1 <- parseJSON @ String s0
    let s2 = takeWhile (/= '@') s1
    let xs = splitOn "-" s2
    pvPackage <- parseJSON $ String $ cs $ intercalate "-" (init xs)
    pvVersion <- parseJSON $ String $ cs $ last xs
    pure PackageVersion { pvPackage, pvVersion }


snapshotMap :: Snapshot -> Map PackageName Version
snapshotMap = M.fromList . map ((pvPackage &&& pvVersion) . hackage) . packages

loadSnapshot :: FilePath -> IO (Either Y.ParseException Snapshot)
loadSnapshot f = Y.decodeFileEither f

data State
  = LookingForLibBounds
  | ProcessingLibBounds
  | Done

io :: MonadIO m => IO a -> m a
io = liftIO

main :: IO ()
main = do
  snapshot_ <- loadSnapshot "../../nightly-2012-12-11.yaml"
  let snapshot = case snapshot_ of
        Left err -> error $ show err
        Right r -> r
  let map = snapshotMap snapshot
  output <- openFile target WriteMode
  let putLine = io . hPutStrLn output
  lines <- lines <$> readFile src
  void $ flip runStateT LookingForLibBounds $ do
    forM_ lines $ \line -> do
      st <- get
      case st of
        LookingForLibBounds -> do
          when (line == "packages:") $
            put ProcessingLibBounds
          putLine line
        ProcessingLibBounds ->
          if line == "# end of packages"
          then do
            put Done
            putLine line
          else
            case p_packageDecl line of
              Just p -> putLine $ handlePackage map p
              Nothing -> putLine line
        Done -> putLine line
  hFlush output
  hClose output
