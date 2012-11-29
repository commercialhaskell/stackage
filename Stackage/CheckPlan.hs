module Stackage.CheckPlan
    ( checkPlan
    ) where

import           Control.Monad        (unless, when)
import           Data.List            (isPrefixOf, sort)
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import           Stackage.InstallInfo
import           Stackage.Types
import           Stackage.Util
import           System.Exit          (ExitCode (ExitFailure, ExitSuccess),
                                       exitWith)
import           System.Process       (readProcessWithExitCode)

data Mismatch = OnlyDryRun String | OnlySimpleList String
    deriving Show

checkPlan :: BuildSettings -> InstallInfo -> IO ()
checkPlan settings ii = do
    (ec, dryRun', stderr) <- readProcessWithExitCode "cabal" (addCabalArgs settings $ "install":"--dry-run":iiPackageList ii) ""
    when (ec /= ExitSuccess || "Warning:" `isPrefixOf` stderr) $ do
        putStr stderr
        putStr dryRun'
        putStrLn "cabal returned a bad result, exiting"
        exitWith ec
    let dryRun = sort $ filter notOptionalCore $ map (takeWhile (/= ' ')) $ drop 2 $ lines dryRun'
    let mismatches = getMismatches dryRun (filter notOptionalCore $ iiPackageList ii)
    unless (null mismatches) $ do
        putStrLn "Found the following mismtaches"
        mapM_ print mismatches
        exitWith $ ExitFailure 1
  where
    optionalCore = Set.fromList $ map packageVersionString $ Map.toList $ iiOptionalCore ii
    notOptionalCore s = not $ s `Set.member` optionalCore

getMismatches :: [String] -> [String] -> [Mismatch]
getMismatches =
    go
  where
    go [] y = map OnlySimpleList y
    go x [] = map OnlyDryRun x
    go (x:xs) (y:ys) =
        case compare x y of
            EQ -> go xs ys
            LT -> OnlyDryRun x     : go xs (y:ys)
            GT -> OnlySimpleList y : go (x:xs) ys
