module Stackage.CheckPlan
    ( checkPlan
    ) where

import           Control.Monad              (unless, when)
import           Data.List                  (isPrefixOf, sort)
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import           Stackage.CheckCabalVersion (checkCabalVersion)
import           Stackage.InstallInfo
import           Stackage.Types
import           Stackage.Util
import           System.Exit                (ExitCode (ExitFailure, ExitSuccess),
                                             exitWith)
import           System.Process             (readProcessWithExitCode)

data Mismatch = OnlyDryRun String | OnlySimpleList String
    deriving Show

checkPlan :: BuildSettings -> BuildPlan -> IO ()
checkPlan settings bp = do
    _ <- checkCabalVersion

    putStrLn "Checking build plan"
    packages <- mapM (replaceTarball $ tarballDir settings) (bpPackageList bp)
    (ec, dryRun', stderr) <- readProcessWithExitCode "cabal"
        ( addCabalArgsOnlyGlobal
        $ "install"
        : "--dry-run"
        : "--max-backjumps=-1"
        : "--reorder-goals"
        : packages
        ) ""
    when (ec /= ExitSuccess || "Warning:" `isPrefixOf` stderr) $ do
        putStr stderr
        putStr dryRun'
        putStrLn "cabal returned a bad result, exiting"
        exitWith ec
    let dryRun = sort $ filter notOptionalCore $ map (takeWhile (/= ' ')) $ drop 2 $ lines dryRun'
    let mismatches = getMismatches dryRun (filter notOptionalCore $ bpPackageList bp)
    unless (null $ filter (not . acceptableMismatch) mismatches) $ do
        putStrLn "Found the following mismatches"
        mapM_ print mismatches
        exitWith $ ExitFailure 1
    putStrLn "Build plan checked, no mismatches"
  where
    optionalCore = Set.fromList $ map packageVersionString $ Map.toList $ bpOptionalCore bp
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

-- | Some mismatches are going to be acceptable. The reasons are described
-- below.
acceptableMismatch :: Mismatch -> Bool
acceptableMismatch m =
    case m of
        -- GHC 7.4 included extensible-extensions as a core package, and
        -- therefore the HP at time of writing (2012.4.0.0) includes it in that
        -- list. However, GHC 7.6 does /not/ include that package. As a result,
        -- we get that package included in the dry run but not our list of
        -- packages to build. See issue #57.
        OnlyDryRun s | "extensible-exceptions-" `isPrefixOf` s -> True
        _ -> False
