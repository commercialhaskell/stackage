module Stackage.InstallInfo
    ( getInstallInfo
    , iiPackageList
    ) where

import Control.Monad (when)
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import           Stackage.Config
import           Stackage.HaskellPlatform
import           Stackage.LoadDatabase
import           Stackage.NarrowDatabase
import           Stackage.Types
import           Stackage.Util
import Data.Version (showVersion)

getInstallInfo :: IO InstallInfo
getInstallInfo = do
    hp <- loadHaskellPlatform
    let allPackages = Map.union stablePackages $ identsToRanges (hplibs hp)
    let totalCore = extraCore `Set.union` Set.map (\(PackageIdentifier p _) -> p) (hpcore hp)
    pdb <- loadPackageDB totalCore allPackages
    final <- narrowPackageDB pdb $ Set.fromList $ Map.keys allPackages

    when verbose $ do
        putStrLn "Basic dependency listing:"
        mapM_ (putStrLn . showDep) $ Map.toList final
    return InstallInfo
        { iiCore = totalCore
        , iiPackages = Map.map fst final
        , iiOptionalCore = Map.fromList $ map (\(PackageIdentifier p v) -> (p, v)) $ Set.toList $ hplibs hp
        }

showDep :: (PackageName, (Version, [PackageName])) -> String
showDep (name, (version, deps)) =
    concat
        [ unP name
        , "-"
        , showVersion version
        , ": "
        , unwords $ map unP deps
        ]
  where
    unP (PackageName p) = p

iiPackageList :: InstallInfo -> [String]
iiPackageList = map packageVersionString . Map.toList . iiPackages
