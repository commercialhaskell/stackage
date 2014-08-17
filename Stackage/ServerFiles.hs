-- | Create the files necessary for Stackage server.
module Stackage.ServerFiles
    ( createHackageFile
    ) where

import Stackage.Util
import Stackage.Types
import qualified Data.Map as Map
import Control.Exception (throwIO)
import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString.Lazy as L
import Control.Arrow (second)
import Control.Monad (unless)
import Distribution.Text (display)
import System.Directory (doesFileExist)
import System.FilePath ((</>), (<.>))
import System.IO (Handle, hPutStrLn, hPutStr)

createHackageFile :: Bool -- ^ inclusive?
                  -> InstallInfo
                  -> String -- ^ GHC version
                  -> String -- ^ date
                  -> Handle -- ^ hackage
                  -> Handle -- ^ tarballs
                  -> IO ()
createHackageFile isInc ii ghcVer date hackageH tarballH = do
    let stackageFP = concat
            [ "../ghc-"
            , ghcVer
            , "-"
            , date
            , if isInc then "-inclusive" else "-exclusive"
            , ".stackage"
            ]
    hPutStr tarballH $ concat
        [ "#!/bin/bash -ex\n\ntar czfv "
        , stackageFP
        , " hackage desc"
        ]
    indextargz <- getTarballName
    indexLBS <- L.readFile indextargz
    loop $ Tar.read indexLBS
    hPutStrLn tarballH ""
    hPutStrLn tarballH $ concat
        [ "runghc ../stackage-upload.hs "
        , stackageFP
        , " unstable-ghc"
        , filter (/= '.') ghcVer
        , if isInc then "-inclusive" else "-exclusive"
        ]

    unless isInc $ do
        -- Add in some OS-specific package/version combos to work with
        -- non-Linux systems.
        hPutStrLn hackageH "hfsevents-0.1.5"
        hPutStrLn hackageH "Win32-notify-0.3"
  where
    selected = Map.fromList . map toStrs . Map.toList $
        fmap spiVersion (iiPackages ii)
        `Map.union` iiOptionalCore ii
        `Map.union` Map.mapMaybe id (iiCore ii)

    toStrs (PackageName name, version) = (name, display version)

    loop Tar.Done = return ()
    loop (Tar.Fail e) = throwIO e
    loop (Tar.Next e es) = go e >> loop es

    go e =
        case parsePair $ Tar.entryPath e of
            Nothing -> return ()
            Just (name, version) ->
                case Map.lookup name selected of
                    Just version'
                        | version == version' -> emit True  name version
                        | otherwise           -> return ()
                    Nothing
                        | isInc               -> emit False name version
                        | otherwise           -> return ()

    emit usePatch name version = do
        exists <- if usePatch then doesFileExist tarball else return False
        if exists
            then hPutStr tarballH $ ' ' : ".." </> tarball
            else hPutStrLn hackageH base
      where
        base = concat [name, "-", version]
        tarball = "patching" </> "tarballs" </> base <.> "tar" <.> "gz"

parsePair :: String -> Maybe (String, String)
parsePair s =
    case splitOn '/' s of
        [name, version, cabal] | name ++ ".cabal" == cabal -> Just (name, version)
        _ -> Nothing

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn c x =
    y : splitOn c z
  where
    (y, z) = second (drop 1) $ break (== c) x
