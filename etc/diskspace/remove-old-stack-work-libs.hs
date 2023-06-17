#!/usr/bin/env stack
-- stack --resolver lts-19.33 script

-- Utility to remove old libs installed under .stack-work/ to save diskspace

import Control.Monad
import Data.List
import System.Directory
import System.FilePath
import System.Environment
import Text.Regex.TDFA

-- keep 2 latest builds
keepBuilds :: Int
keepBuilds = 2

main :: IO ()
main = do
  args <- getArgs
  case args of
    [stream] -> do
      home <- getHomeDirectory
      withCurrentDirectory (home </> "stackage/automated/work" </> stream </> "unpack-dir") $ do
        cleanStackWorkInstall
        cleanStackWorkPackages
    _ -> error "arg should be 'lts' or 'nightly'"

  -- navigates to: .stack-work/install/x86_64-linux*/*/*/lib/x86_64-linux-ghc-*
cleanStackWorkInstall :: IO ()
cleanStackWorkInstall =
  withCurrentDirectory ".stack-work/install"
  $ withOneDirectory_ -- "x86_64-linux*"
  $ withOneDirectory_ -- hash
  $ withOneDirectory $ \ghcver ->
  withCurrentDirectory ("lib" </> "x86_64-linux-ghc-" ++ ghcver) $ do
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
          if length p < 25
          then error $ p ++ " too short to be in correct name-version-hash format"
          else extractNameInternal p

    extractNameInternal :: String -> String
    extractNameInternal p =
      let (name,match,internal) = p =~ "-[0-9.]+-[0-9A-Za-z]{19,22}" :: (String, String, String)
      in if null match || null name then error $ p ++ " not in correct name-version-hash format"
         else name ++ internal

    samePkgDynLib d1 d2 = pkgDynName d1 == pkgDynName d2
      where
        pkgDynName p =
          if length p < 42
          then error $ p ++ " too short to be libHSname-version-hash-ghc*.so format"
          else (extractNameInternal . removeDashSegment) p

    removeDashSegment = dropWhileEnd (/= '-')

removeOlder remover files = do
  oldfiles <- drop keepBuilds . reverse <$> sortByAge files
  mapM_ remover oldfiles
  where
    sortByAge files = do
      timestamps <- mapM getModificationTime files
      let fileTimes = zip files timestamps
      return $ map fst $ sortBy compareSnd fileTimes

    compareSnd (_,t1) (_,t2) = compare t1 t2

-- navigates to:
-- unpacked/*/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.8.1.0/build/
cleanStackWorkPackages :: IO ()
cleanStackWorkPackages =
  withCurrentDirectory "unpacked" $ do
  pkgs <- listDirectory "."
  forM_ pkgs $ \pkg -> do
    withCurrentDirectory ".stack-work/dist/x86_64-linux-tinfo6"
    $ withOneDirectory_ -- "Cabal-3.8.1.0"
    $ withCurrentDirectory "build" $ do
      ls <- sort <$> listDirectory "."
      files <- filterM doesFileExist ls
      let (dynlibs,statlibs) = partition (".so" `isExtensionOf`) files
      removeOlder removeFile dynlibs
      removeOlder removeFile statlibs

withOneDirectory_ act = do
  ls <- listDirectory "."
  case ls of
    [l] -> withCurrentDirectory l act
    _ -> error $ "more than one directory found: " ++ unwords ls

withOneDirectory act = do
  ls <- listDirectory "."
  case ls of
    [l] -> withCurrentDirectory l $ act l
    _ -> error $ "more than one directory found: " ++ unwords ls
