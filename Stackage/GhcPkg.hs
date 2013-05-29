module Stackage.GhcPkg where

import Stackage.Types
import System.Process
import           Distribution.Text (simpleParse)
import Data.Char (isSpace)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Control.Monad (guard)

getGlobalPackages :: IO (Set PackageIdentifier)
getGlobalPackages = do
    -- Account for a change in command line option name
    versionOutput <- readProcess "ghc-pkg" ["--version"] ""
    let arg = fromMaybe "--no-user-package-db" $ do
            verS:_ <- Just $ reverse $ words versionOutput
            v76 <- simpleParse "7.6"
            ver <- simpleParse verS
            guard $ ver < (v76 :: Version)
            return "--no-user-package-conf"
    output <- readProcess "ghc-pkg" [arg, "list"] ""
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
