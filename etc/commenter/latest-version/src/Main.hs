module Main where

import Data.List
import Distribution.Types.PackageName
import Distribution.Types.Version
import Pantry
import RIO
import System.Environment
import qualified Data.Map as Map

main :: IO ()
main = do
  args <- getArgs
  (onlyVersion, packages) <- pure $ case args of
    "only-version" : packages -> (True, packages)
    packages -> (False, packages)
  runPantryApp $ liftIO . putStrLn . unlines_ =<< mapM (latestVersion onlyVersion) packages
  where
    -- unlines adds an extra trailing newline which can be annoying...
    unlines_ = intercalate "\n"

latestVersion :: (HasPantryConfig env, HasLogFunc env) => Bool -> String -> RIO env String
latestVersion onlyVersion name = fmap (displayVersion onlyVersion) . getVersion . mkPackageName $ name
  where
    showVersion = intercalate "." . map show . versionNumbers . fst . head . Map.toDescList
    getVersion = getHackagePackageVersions YesRequireHackageIndex UsePreferredVersions
    displayVersion True v = showVersion v
    displayVersion False v = name <> "-" <> showVersion v
