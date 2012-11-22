import           Control.Monad            (when, unless, foldM)
import           Data.List                (sort)
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import           Data.Version             (showVersion)
import           Stackage.HaskellPlatform
import           Stackage.LoadDatabase
import           Stackage.NarrowDatabase
import           Stackage.PackageList
import           Stackage.Types
import           Stackage.Util
import           System.Directory         (doesDirectoryExist, removeDirectoryRecursive, removeFile, createDirectory)
import           System.Process           (readProcess, waitForProcess, runProcess)
import System.Exit (ExitCode (ExitSuccess), exitWith)
import System.FilePath ((</>), (<.>))
import System.IO (IOMode (WriteMode, AppendMode), withBinaryFile)

data Mismatch = OnlyDryRun String | OnlySimpleList String
    deriving Show

extraCore :: Set PackageName
extraCore = Set.singleton $ PackageName "binary"

-- Test suites which are expected to fail for some reason. The test suite
-- will still be run and logs kept, but a failure will not indicate an
-- error in our package combination.
expectedFailures :: Set PackageName
expectedFailures = Set.fromList $ map PackageName
    [ -- Requires an old version of WAI and Warp for tests
      "HTTP"
      -- Requires a special hspec-meta which is not yet available from
      -- Hackage.
    , "hspec"
    ]

main :: IO ()
main = do
    userPackages <- loadPackageList "package-list.txt"
    hp <- loadHaskellPlatform
    let allPackages = Map.union userPackages $ identsToRanges (hplibs hp)
    pdb <- loadPackageDB (extraCore `Set.union` Set.map (\(PackageIdentifier p _) -> p) (hpcore hp)) allPackages
    final <- narrowPackageDB pdb $ Set.fromList $ Map.keys allPackages
    let simpleList = map (\(PackageName p, v) -> p ++ "-" ++ showVersion v) $ Map.toList final
    writeFile "to-install.txt" $ unlines simpleList

    rm_r "cabal-dev"

    dryRun' <- readProcess "cabal-dev" ("install":"--dry-run":"-fnetwork23":simpleList) ""
    writeFile "dry-run.txt" dryRun'
    let dryRun = sort $ drop 2 $ lines dryRun'
    let mismatches = getMismatches dryRun simpleList
    if null mismatches
        then do
            putStrLn "No mismatches, good to go!"
            ph <- withBinaryFile "build.log" WriteMode $ \handle -> runProcess "cabal-dev" ("install":"-fnetwork23":simpleList) Nothing Nothing Nothing (Just handle) (Just handle)
            ec <- waitForProcess ph
            unless (ec == ExitSuccess) $ exitWith ec
            putStrLn "Environment built, beginning individual test suites"
            let testdir = "runtests"
            rm_r testdir
            createDirectory testdir
            allPass <- foldM (runTestSuite testdir) True $ zip simpleList $ Map.toList final
            if allPass
                then putStrLn "All test suites pass"
                else putStrLn "There were failures, please see the logs in runtests"
        else do
            putStrLn "Found the following mismtaches"
            mapM_ print mismatches

rm_r :: FilePath -> IO ()
rm_r fp = do
    exists <- doesDirectoryExist fp
    when exists $ removeDirectoryRecursive fp

runTestSuite :: FilePath -> Bool -> (String, (PackageName, Version)) -> IO Bool
runTestSuite testdir prevPassed (package, (packageName, _)) = do
    passed <- do
        ph1 <- getHandle WriteMode $ \handle -> runProcess "cabal-dev" ["unpack", package] (Just testdir) Nothing Nothing (Just handle) (Just handle)
        ec1 <- waitForProcess ph1
        if (ec1 /= ExitSuccess)
            then return False
            else do
                putStrLn dir
                ph2 <- getHandle AppendMode $ \handle -> runProcess "cabal-dev" ["-s", "../../cabal-dev", "configure", "--enable-tests"] (Just dir) Nothing Nothing (Just handle) (Just handle)
                ec2 <- waitForProcess ph2
                if (ec2 /= ExitSuccess)
                    then return False
                    else do
                        ph3 <- getHandle AppendMode $ \handle -> runProcess "cabal-dev" ["build"] (Just dir) Nothing Nothing (Just handle) (Just handle)
                        ec3 <- waitForProcess ph3
                        if (ec3 /= ExitSuccess)
                            then return False
                            else do
                                ph4 <- getHandle AppendMode $ \handle -> runProcess "cabal-dev" ["test"] (Just dir) Nothing Nothing (Just handle) (Just handle)
                                ec4 <- waitForProcess ph4
                                return $ ec4 == ExitSuccess
    let expectedFailure = packageName `Set.member` expectedFailures
    if passed
        then do
            removeFile logfile
            when expectedFailure $ putStrLn $ package ++ " passed, but I didn't think it would."
        else putStrLn $ "Test suite failed: " ++ package
    rm_r dir
    return $! prevPassed && (passed || expectedFailure)
  where
    logfile = testdir </> package <.> "log"
    dir = testdir </> package
    getHandle mode = withBinaryFile logfile mode

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
