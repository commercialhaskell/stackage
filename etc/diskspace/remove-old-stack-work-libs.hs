#!/usr/bin/env stack
-- stack --resolver lts-14 script

-- Utility to remove old libs installed under .stack-work/ to save diskspace

-- Should be run in:
-- work/*/unpack-dir/.stack-work/install/x86_64-linux/*/*/lib/x86_64-linux-ghc-*

import Data.List
import System.Directory
import System.FilePath

main = do
  files <- sort <$> listDirectory "."
  let (dynlibs,libdirs) = partition (".so" `isExtensionOf`) files
      pkglibdirs = groupBy samePkgLibDir libdirs
      pkgdynlibs = groupBy samePkgDynLib dynlibs
  mapM_ (removeOlder removeDirectoryRecursive) pkglibdirs
  mapM_ (removeOlder removeFile) pkgdynlibs
  where
    samePkgLibDir l1 l2 = pkgDirName l1 == pkgDirName l2
      where
        pkgDirName p =
          if length p < 26 || countDashes p < 2
          then error $ p ++ " not in name-version-hash format"
          else (removeDashSegment . removeHashSegment) p

    countDashes = length . filter (== '-')

    removeDashSegment = init . dropWhileEnd (/= '-')

    removeHashSegment p = let nv_ = dropEnd 22 p in
      if last nv_ == '-' then init nv_
      else error $ p ++ " has incorrect hash format"

    samePkgDynLib d1 d2 = pkgDynName d1 == pkgDynName d2
      where
        pkgDynName p =
          if length p < 43 || countDashes p < 3
          then error $ p ++ " not in libHSname-version-hash-ghc*.so format"
          else (removeDashSegment . removeHashSegment . removeDashSegment) p

    removeOlder remover files = do
      -- keep 2 latest builds
      oldfiles <- drop 2 . reverse <$> sortByAge files
      mapM_ remover oldfiles

    sortByAge files = do
      timestamps <- mapM getModificationTime files
      let fileTimes = zip files timestamps
      return $ map fst $ sortBy compareSnd fileTimes

    compareSnd (_,t1) (_,t2) = compare t1 t2

-- from Data.List.Extra
dropEnd :: Int -> [a] -> [a]
dropEnd i xs = f xs (drop i xs)
    where f (x:xs) (y:ys) = x : f xs ys
          f _ _ = []
