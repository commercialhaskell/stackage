module Stackage.Tarballs
    ( makeTarballs
    ) where

import qualified Codec.Archive.Tar    as Tar
import qualified Data.ByteString.Lazy as L
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import           Stackage.Types
import           Stackage.Util
import           System.Directory     (createDirectoryIfMissing)
import           System.FilePath      (takeDirectory)

makeTarballs :: BuildPlan -> IO ()
makeTarballs bp = do
    putStrLn "Building tarballs"
    tarName <- getTarballName
    origEntries <- fmap Tar.read $ L.readFile tarName
    (stableEntries, extraEntries) <- loop id id origEntries

    (stableTar, extraTar) <- getStackageTarballNames

    createDirectoryIfMissing True $ takeDirectory stableTar
    L.writeFile stableTar $ Tar.write stableEntries

    createDirectoryIfMissing True $ takeDirectory extraTar
    L.writeFile extraTar $ Tar.write extraEntries
  where
    -- Using "error . show" for compatibility with tar 0.3 and 0.4
    loop _ _ (Tar.Fail err) = error $ show err
    loop stable extra Tar.Done = return (stable [], extra [])
    loop stable extra (Tar.Next e es) =
        loop stable' extra' es
      where
        (stable', extra') =
            case getPackageVersion e of
                Nothing -> (stable, extra)
                Just (package, version) ->
                    case Map.lookup package $ bpPackages bp of
                        Just spi
                            | version == spiVersion spi -> (stable . (e:), extra)
                            | otherwise -> (stable, extra)
                        Nothing
                            | package `Set.member` bpCore bp -> (stable, extra)
                            | otherwise -> (stable, extra . (e:))
