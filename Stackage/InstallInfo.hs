module Stackage.InstallInfo
    ( getInstallInfo
    , iiPackageList
    ) where

import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import           Stackage.Config
import           Stackage.HaskellPlatform
import           Stackage.LoadDatabase
import           Stackage.NarrowDatabase
import           Stackage.Types
import           Stackage.Util
import Data.Version (showVersion)

getInstallInfo :: BuildSettings -> IO InstallInfo
getInstallInfo settings = do
    hp <- loadHaskellPlatform
    let allPackages = Map.union (stablePackages settings) $ identsToRanges (hplibs hp)
    let totalCore = extraCore settings `Set.union` Set.map (\(PackageIdentifier p _) -> p) (hpcore hp)
    pdb <- loadPackageDB totalCore allPackages
    final <- narrowPackageDB pdb $ Set.fromList $ Map.toList $ Map.map snd $ allPackages

    putStrLn "Printing build plan to build-plan.log"
    writeFile "build-plan.log" $ unlines $ map showDep $ Map.toList final
    return InstallInfo
        { iiCore = totalCore
        , iiPackages = Map.map (\(v, _, m) -> (v, m)) final
        , iiOptionalCore = Map.fromList $ map (\(PackageIdentifier p v) -> (p, v)) $ Set.toList $ hplibs hp
        , iiPackageDB = pdb
        }

showDep :: (PackageName, (Version, [PackageName], Maintainer)) -> String
showDep (name, (version, deps, Maintainer m)) =
    concat
        [ unP name
        , "-"
        , showVersion version
        , " ("
        , m
        , ")"
        , ": "
        , unwords $ map unP deps
        ]
  where
    unP (PackageName p) = p

iiPackageList :: InstallInfo -> [String]
iiPackageList = map packageVersionString . Map.toList . Map.map fst . iiPackages
