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
import Distribution.Text (display)
import System.IO (Handle, hPutStrLn)

createHackageFile :: InstallInfo -> Handle -> IO ()
createHackageFile ii h = do
    indextargz <- getTarballName
    indexLBS <- L.readFile indextargz
    loop $ Tar.read indexLBS
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
                    Just version' | version /= version' -> return ()
                    _ -> hPutStrLn h $ concat [name, "-", version]

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
