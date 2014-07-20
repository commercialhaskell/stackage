module Stackage.GhcPkg where

import Stackage.Types
import System.Process
import           Distribution.Text (simpleParse)
import           Distribution.Version (Version (Version))
import Data.Char (isSpace)
import qualified Data.Set as Set

getPackages :: [String] -> GhcMajorVersion -> IO (Set PackageIdentifier)
getPackages extraArgs version = do
    output <- readProcess "ghc-pkg" (extraArgs ++ [arg, "list"]) ""
    fmap Set.unions $ mapM parse $ drop 1 $ lines output
  where
    -- Account for a change in command line option name
    arg
        | version >= GhcMajorVersion 7 6 = "--no-user-package-db"
        | otherwise = "--no-user-package-conf"
    parse s =
        case clean s of
            "" -> return Set.empty
            s' ->
                case simpleParse s' of
                    Just x -> return $ Set.singleton x
                    Nothing -> error $ "Could not parse ghc-pkg output: " ++ show s
    clean = stripParens . dropWhile isSpace . reverse . dropWhile isSpace . reverse
    stripParens x@('(':_:_)
        | last x == ')' = tail $ init $ x
    stripParens x = x

getGlobalPackages :: GhcMajorVersion -> IO (Set PackageIdentifier)
getGlobalPackages version = getPackages [] version

getDBPackages :: [FilePath] -> GhcMajorVersion -> IO (Set PackageIdentifier)
getDBPackages [] _ = return Set.empty
getDBPackages packageDirs version =
    getPackages (map packageDbArg packageDirs) version
  where
    packageDbArg db
        | version >= GhcMajorVersion 7 6 = "--package-db=" ++ db
        | otherwise = "--package-conf" ++ db

getGhcVersion :: IO GhcMajorVersion
getGhcVersion = do
    versionOutput <- readProcess "ghc-pkg" ["--version"] ""
    maybe (error $ "Invalid version output: " ++ show versionOutput) return $ do
        verS:_ <- Just $ reverse $ words versionOutput
        Version (x:y:_) _ <- simpleParse verS
        return $ GhcMajorVersion x y
