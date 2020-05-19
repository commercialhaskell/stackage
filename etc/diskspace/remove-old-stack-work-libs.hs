#!/usr/bin/env stack
-- stack --resolver lts-14 script

-- Utility to remove old libs installed under .stack-work/ to save diskspace

-- Should be run in:
-- work/*/unpack-dir/.stack-work/install/x86_64-linux/*/*/lib/x86_64-linux-ghc-*

import Data.List
import Data.List.Extra
import System.Directory
import System.FilePath

main = do
  files <- sort <$> listDirectory "."
  let (dynlibs,libdirs) = partition (".so" `isExtensionOf`) files
      pkglibdirs = groupBy samePkgLibDir $
                   filter (not . ("-internal" `isSuffixOf`)) libdirs
      pkgdynlibs = groupBy samePkgDynLib $
                   filter (not . ("-internal-" `isInfixOf`)) dynlibs
  mapM_ (removeOlder removeDirectoryRecursive) pkglibdirs
  mapM_ (removeOlder removeFile) pkgdynlibs
  where
    samePkgLibDir l1 l2 = pkgDirName l1 == pkgDirName l2
      where
        pkgDirName p =
          if length p < 25
          then error $ p ++ " too short to be in correct name-version-hash format"
          else (removeDashSegment . removeHashSegment) p

    removeHashSegment p =
      let dashes = elemIndices '-' p in
        if length dashes < 2
        then error $ p ++ " not in name-version-hash format"
        else let final = last dashes in
          if length p - final `elem` [23,22,21] then take final p
          else error $ p ++ " has incorrect hash length"

    removeDashSegment = init . dropWhileEnd (/= '-')

    samePkgDynLib d1 d2 = pkgDynName d1 == pkgDynName d2
      where
        pkgDynName p =
          if length p < 42
          then error $ p ++ " too short to be libHSname-version-hash-ghc*.so format"
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
