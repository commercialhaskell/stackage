module Stackage.ModuleNameConflict
    ( ModuleNameConflicts
    , getModuleNameConflicts
    , renderModuleNameConflicts
    , parseModuleNameConflicts
    ) where

import Distribution.Simple.Configure (configCompiler, getInstalledPackages)
import Distribution.Simple.Compiler (CompilerFlavor (GHC), PackageDB (GlobalPackageDB, SpecificPackageDB))
import Distribution.Verbosity (normal)
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Simple.PackageIndex (moduleNameIndex)
import Distribution.InstalledPackageInfo (sourcePackageId)
import Distribution.Package (PackageIdentifier (PackageIdentifier), PackageName (PackageName))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (intercalate)
import Distribution.ModuleName (components)

type ModuleNameConflicts = Map.Map (Set.Set String) (Set.Set String)

getModuleNameConflicts :: FilePath -> IO ModuleNameConflicts
getModuleNameConflicts path = do
    (compiler, progConfig) <-
        configCompiler (Just GHC) Nothing Nothing defaultProgramConfiguration normal
    let stack =
            [ GlobalPackageDB
            , SpecificPackageDB path
            ]
    packageIndex <- getInstalledPackages normal compiler stack progConfig
    let modMap = moduleNameIndex packageIndex
        packageName (PackageIdentifier (PackageName x) _) = x
        simpleMN = intercalate "." . components
        overlaps = Map.unionsWith Set.union
                 $ map (\(mn, pkgs) -> Map.singleton pkgs (Set.singleton $ simpleMN mn))
                 $ Map.toList
                 $ Map.filter (\x -> Set.size x > 1)
                 $ Map.map Set.fromList
                 $ fmap (map (packageName . sourcePackageId)) modMap
    return overlaps

renderModuleNameConflicts :: ModuleNameConflicts -> String
renderModuleNameConflicts =
    unlines . map (unwords . Set.toList) . concatMap (\(x, y) -> [x, y]) . Map.toList

parseModuleNameConflicts :: String -> ModuleNameConflicts
parseModuleNameConflicts =
    Map.fromList . toPairs . map (Set.fromList . words) . lines
  where
    toPairs [] = []
    toPairs [_] = []
    toPairs (x:y:z) = (x, y) : toPairs z
