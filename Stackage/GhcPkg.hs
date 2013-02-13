module Stackage.GhcPkg where

import Stackage.Types
import System.Process
import           Distribution.Text (simpleParse)
import Data.Char (isSpace)
import qualified Data.Set as Set

getGlobalPackages :: IO (Set PackageIdentifier)
getGlobalPackages = do
    output <- readProcess "ghc-pkg" ["--no-user-package-db", "list"] ""
    fmap Set.unions $ mapM parse $ drop 1 $ lines output
  where
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
