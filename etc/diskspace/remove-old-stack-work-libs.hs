#!/usr/bin/env stack
-- stack --resolver lts-14 script

-- Utility to remove old libs installed under .stack-work/ to save diskspace

-- Should be run in:
-- work/*/unpack-dir/.stack-work/install/x86_64-linux/*/*/lib/x86_64-linux-ghc-*

import Data.List
import System.Directory

main = do
  files <- listDirectory "."
  (libdirs,dynlibs) <- partitionM doesDirectoryExist files
  let pkglibdirs = groupBy samePkgLibDir libdirs
      pkgdynlibs = groupBy samePkgDynLib dynlibs
  mapM_ (removeOlder removeDirectoryRecursive) pkglibdirs
  mapM_ (removeOlder removeFile) pkgdynlibs
  where
    samePkgLibDir l1 l2 = pkgDirName l1 == pkgDirName l2
      where
        pkgDirName p =
          if countDashes p < 2
          then error $ p ++ " not in name-version-hash format"
          else (removeDashSegment . removeDashSegment) p

    countDashes = length . filter (== '-')

    removeDashSegment = init . dropWhileEnd (/= '-')

    samePkgDynLib d1 d2 = pkgDynName d1 == pkgDynName d2
      where
        pkgDynName p =
          if countDashes p < 3
          then error $ p ++ " not in libname-version-hash-ghc*.so format"
          else (removeDashSegment . removeDashSegment . removeDashSegment) p

    removeOlder remover files = do
      oldfiles <- drop 2 . reverse <$> sortByAge files
      mapM_ remover oldfiles

    sortByAge files = do
      timestamps <- mapM getModificationTime files
      let fileTimes = zip files timestamps
      return $ map fst $ sortBy compareSnd fileTimes

    compareSnd (_,t1) (_,t2) = compare t1 t2

-- borrowed from Control.Monad.Extra
partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f [] = pure ([], [])
partitionM f (x:xs) = do
    res <- f x
    (as,bs) <- partitionM f xs
    pure ([x | res]++as, [x | not res]++bs)
