module Main where

import Data.List
import Distribution.Types.PackageName
import Distribution.Types.Version
import Pantry
import RIO
import System.Environment
import qualified Data.Map as Map

main :: IO ()
main =
  runPantryApp $
    liftIO . putStrLn
    . intercalate "." . map show . versionNumbers
    . fst . head . Map.toDescList
    =<< getHackagePackageVersions YesRequireHackageIndex IgnorePreferredVersions
    . mkPackageName =<< head <$> liftIO getArgs
