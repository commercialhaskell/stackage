module Stackage.InstallInfo
    ( getInstallInfo
    , iiPackageList
    ) where

import           Stackage.Types
import           Stackage.Util
import           Stackage.Config
import           Stackage.LoadDatabase
import           Stackage.NarrowDatabase
import           Stackage.HaskellPlatform
import qualified Data.Map as Map
import qualified Data.Set as Set

getInstallInfo :: IO InstallInfo
getInstallInfo = do
    hp <- loadHaskellPlatform
    let allPackages = Map.union stablePackages $ identsToRanges (hplibs hp)
    let totalCore = extraCore `Set.union` Set.map (\(PackageIdentifier p _) -> p) (hpcore hp)
    pdb <- loadPackageDB totalCore allPackages
    final <- narrowPackageDB pdb $ Set.fromList $ Map.keys allPackages
    return InstallInfo
        { iiCore = totalCore
        , iiPackages = final
        }

iiPackageList :: InstallInfo -> [String]
iiPackageList = map packageVersionString . Map.toList . iiPackages
