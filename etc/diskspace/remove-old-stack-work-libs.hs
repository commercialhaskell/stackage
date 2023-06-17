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
        putStrLn ""
        cleanStackWorkPackages
    _ -> error "arg should be 'lts-XX' or 'nightly'"

  -- navigates to: .stack-work/install/x86_64-linux*/*/*/lib/x86_64-linux-ghc-*
cleanStackWorkInstall :: IO ()
cleanStackWorkInstall =
  withCurrentDirectory ".stack-work/install"
  $ withOneDirectory_ -- "x86_64-linux*"
  $ withOneDirectory_ -- hash
  $ withOneDirectory $ \ghcver ->
  withCurrentDirectory ("lib" </> "x86_64-linux-ghc-" ++ ghcver) $ do
  getCurrentDirectory >>= putStrLn
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
      let (name,match',internal) = p =~ "-[0-9.]+-[0-9A-Za-z]{19,22}" :: (String, String, String)
      in if null match' || null name then error $ p ++ " not in correct name-version-hash format"
         else name ++ internal

    samePkgDynLib d1 d2 = pkgDynName d1 == pkgDynName d2
      where
        pkgDynName p =
          if length p < 42
          then error $ p ++ " too short to be libHSname-version-hash-ghc*.so format"
          else (extractNameInternal . removeDashSegment) p

    removeDashSegment = dropWhileEnd (/= '-')


removeOlder :: (FilePath -> IO ()) -> [FilePath] -> IO ()
removeOlder remover files = do
  oldfiles <- drop keepBuilds . reverse <$> sortByAge files
  mapM_ remover oldfiles

sortByAge :: [FilePath] -> IO [FilePath]
sortByAge files = do
  timestamps <- mapM getModificationTime files
  let fileTimes = zip files timestamps
  return $ map fst $ sortBy compareSnd fileTimes
  where
    compareSnd (_,t1) (_,t2) = compare t1 t2

-- navigates to:
-- unpacked/*/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.8.1.0/build/
cleanStackWorkPackages :: IO ()
cleanStackWorkPackages =
  withCurrentDirectory "unpacked" $ do
  getCurrentDirectory >>= putStrLn
  pkgs <- listDirectory "."
  forM_ pkgs $ \pkg ->
    withCurrentDirectory (pkg </> ".stack-work/dist") $ do
    -- [(dyn,stat)]
    libs <- do
      platforms <- listDirectory "." -- "x86_64-linux-tinfo6*"
      forM platforms $ \pl ->
        withCurrentDirectory pl $
        withOneDirectory -- "Cabal-*"
        $ \cbl ->
        withCurrentDirectory "build" $ do
        ls <- sort <$> listDirectory "."
        files <- filterM doesFileExist ls
        let (dynlibs,others) = partition (".so" `isExtensionOf`) files
            statlibs = filter (".a" `isExtensionOf`) others
        let dir = pl </> cbl </> "build"
        return (map (dir </>) dynlibs, map (dir </>) statlibs)
    removeOlder removeFile $ concatMap fst libs
    removeOlder removeFile $ concatMap snd libs

withOneDirectory_ :: IO a -> IO a
withOneDirectory_ act = do
  ls <- listDirectory "."
  case ls of
    [l] -> withCurrentDirectory l act
    _ -> do
      cwd <- getCurrentDirectory
      error $ "more than one directory found in " ++ cwd ++ ": " ++ unwords ls

withOneDirectory :: (FilePath -> IO a) -> IO a
withOneDirectory act = do
  ls <- listDirectory "."
  case ls of
    [l] -> withCurrentDirectory l $ act l
    _ -> do
      cwd <- getCurrentDirectory
      error $ "more than one directory found in " ++ cwd ++ ": " ++ unwords ls
