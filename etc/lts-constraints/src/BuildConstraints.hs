{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS -Wno-name-shadowing #-}
module BuildConstraints where

import Control.Arrow
import Data.Char
import Data.Maybe
import Data.String.Conversions
import Distribution.Text (display, simpleParse)
import Distribution.Types.VersionRange (VersionRange, normaliseVersionRange, anyVersion, intersectVersionRanges, majorBoundVersion, earlierVersion)
import RIO.Map (Map)
import RIO.Text (Text)
import qualified Data.Text as T
import qualified Distribution.Types.Version as C (mkVersion)
import qualified RIO.Map as M

import Types

takeDropWhile :: (Char -> Bool) -> Text -> Maybe (Text, Text)
takeDropWhile p s = if T.null a then Nothing else Just (a, b)
  where
    (a, b) = takeDropWhile_ p s

takeDropWhile_ :: (Char -> Bool) -> Text -> (Text, Text)
takeDropWhile_ p s = (T.takeWhile p s, T.dropWhile p s)

takePrefix :: Text -> Text -> Maybe (Text, Text)
takePrefix p s =
  if p `T.isPrefixOf` s
  then Just (p, T.drop (T.length p) s)
  else Nothing

takePackageName :: Text -> Maybe (PackageName, Text)
takePackageName = fmap (first mkPackageName) . takeDropWhile (/= ' ')

maybeTakeVersionRange :: Text -> (Maybe VersionRange, Text)
maybeTakeVersionRange s = (simpleParse $ cs range, comment)
  where
    (range, comment) = takeDropWhile_ (/= '#') s

parsePackageDecl :: Text -> Maybe PackageDecl
parsePackageDecl s = do
  (prefix, s0) <- takePrefix "        - " s
  (package, s1) <- takePackageName s0
  let (range, s2) = maybeTakeVersionRange s1
  pure PackageDecl { prefix, package, range = fromMaybe anyVersion range, suffix = s2 }

handlePackage :: Map PackageName Version -> PackageDecl -> Text
handlePackage snap PackageDecl { prefix, package, range, suffix } =
  prefix <> (cs . display . unPackageName) package <> rng <> suff
  where
    suff :: Text
    suff = if T.null suffix then suffix else " " <> suffix

    rng = case (majorBoundVersion . unVersion <$> snapshotVersion) `intersect` range of
      Just rng | rng == anyVersion -> ""
      Nothing -> ""
      Just rng -> (" " <>) . (\(a,b) -> a <> " " <> b) . takeDropWhile_ (not . isDigit) . cs $ display rng
    snapshotVersion = M.lookup package snap

    intersect Nothing _ = Just . earlierVersion $ C.mkVersion [0] -- package not in snapshot
    intersect (Just a) b =
      if b == anyVersion -- drop `&& -any`
      then Just a
      else Just $ normaliseVersionRange (intersectVersionRanges a b)
