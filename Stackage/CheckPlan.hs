module Stackage.CheckPlan
    ( checkPlan
    ) where

import           Control.Monad            (unless)
import           Data.List                (sort)
import           Stackage.Types
import           Stackage.InstallInfo
import           System.Process           (readProcess)
import System.Exit (ExitCode (ExitFailure), exitWith)

data Mismatch = OnlyDryRun String | OnlySimpleList String
    deriving Show

checkPlan :: InstallInfo -> IO ()
checkPlan ii = do
    dryRun' <- readProcess "cabal-dev" ("install":"--dry-run":"-fnetwork23":iiPackageList ii) ""
    let dryRun = sort $ drop 2 $ lines dryRun'
    let mismatches = getMismatches dryRun (iiPackageList ii)
    unless (null mismatches) $ do
        putStrLn "Found the following mismtaches"
        mapM_ print mismatches
        exitWith $ ExitFailure 1

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
