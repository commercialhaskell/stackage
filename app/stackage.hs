{-# LANGUAGE RecordWildCards #-}
import           Stackage.Types
import           Stackage.Build     (build, defaultBuildSettings)
import           Stackage.Init      (stackageInit)
import           Stackage.Util      (allowPermissive)
import           Stackage.Select    (defaultSelectSettings, select)
import           Stackage.CheckPlan (checkPlan)
import           System.Environment (getArgs, getProgName)
import           Data.Set           (fromList)
import           System.IO          (hFlush, stdout)
import Stackage.BuildPlan (readBuildPlan, writeBuildPlan)
import Stackage.Test (runTestSuites)
import Stackage.Tarballs (makeTarballs)

data SelectArgs = SelectArgs
    { excluded :: [String]
    , noPlatform :: Bool
    , onlyPermissive :: Bool
    , allowed :: [String]
    , buildPlanDest :: FilePath
    }

parseSelectArgs :: [String] -> IO SelectArgs
parseSelectArgs =
    loop SelectArgs
        { excluded = []
        , noPlatform = False
        , onlyPermissive = False
        , allowed = []
        , buildPlanDest = defaultBuildPlan
        }
  where
    loop x [] = return x
    loop x ("--exclude":y:rest) = loop x { excluded = y : excluded x } rest
    loop x ("--no-platform":rest) = loop x { noPlatform = True } rest
    loop x ("--only-permissive":rest) = loop x { onlyPermissive = True } rest
    loop x ("--allow":y:rest) = loop x { allowed = y : allowed x } rest
    loop x ("--build-plan":y:rest) = loop x { buildPlanDest = y } rest
    loop _ (y:_) = error $ "Did not understand argument: " ++ y

data BuildArgs = BuildArgs
    { sandbox :: String
    , buildPlanSrc :: FilePath
    , extraArgs' :: [String] -> [String]
    }

parseBuildArgs :: [String] -> IO BuildArgs
parseBuildArgs =
    loop BuildArgs
        { sandbox = sandboxRoot defaultBuildSettings
        , buildPlanSrc = defaultBuildPlan
        , extraArgs' = id
        }
  where
    loop x [] = return x
    loop x ("--sandbox":y:rest) = loop x { sandbox = y } rest
    loop x ("--build-plan":y:rest) = loop x { buildPlanSrc = y } rest
    loop x ("--arg":y:rest) = loop x { extraArgs' = extraArgs' x . (y:) } rest
    loop _ (y:_) = error $ "Did not understand argument: " ++ y

defaultBuildPlan :: FilePath
defaultBuildPlan = "build-plan.txt"

withBuildSettings :: [String] -> (BuildSettings -> BuildPlan -> IO a) -> IO a
withBuildSettings args f = do
    BuildArgs {..} <- parseBuildArgs args
    bp <- readBuildPlan buildPlanSrc
    let settings = defaultBuildSettings
            { sandboxRoot = sandbox
            , extraArgs = extraArgs' $ extraArgs defaultBuildSettings
            }
    f settings bp

main :: IO ()
main = do
    args <- getArgs
    case args of
        "select":rest -> do
            SelectArgs {..} <- parseSelectArgs rest
            bp <- select
                defaultSelectSettings
                    { excludedPackages = fromList $ map PackageName excluded
                    , requireHaskellPlatform = not noPlatform
                    , allowedPackage =
                        if onlyPermissive
                            then allowPermissive allowed
                            else const $ Right ()
                    }
            writeBuildPlan buildPlanDest bp
        ("check":rest) -> withBuildSettings rest $ const checkPlan
        ("build":rest) -> withBuildSettings rest build
        ("test":rest) -> withBuildSettings rest runTestSuites
        ("tarballs":rest) -> withBuildSettings rest $ const makeTarballs
        ["init"] -> do
            putStrLn "Note: init isn't really ready for prime time use."
            putStrLn "Using it may make it impossible to build stackage."
            putStr "Are you sure you want continue (y/n)? "
            hFlush stdout
            x <- getLine
            case x of
                c:_ | c `elem` "yY" -> stackageInit
                _ -> putStrLn "Probably a good decision, exiting."
        ["update"] -> stackageInit >> error "FIXME update"
        _ -> do
            pn <- getProgName
            putStrLn $ "Usage: " ++ pn ++ " <command>"
            putStrLn "Available commands:"
            --putStrLn "    update              Download updated Stackage databases. Automatically calls init."
            --putStrLn "    init                Initialize your cabal file to use Stackage"
            putStrLn "    select [--no-clean] [--no-platform] [--exclude package...] [--only-permissive] [--allow package] [--build-plan file]"
            putStrLn "    check [--build-plan file] [--sandbox rootdir] [--arg cabal-arg]"
            putStrLn "    build [--build-plan file] [--sandbox rootdir] [--arg cabal-arg]"
            putStrLn "    test [--build-plan file] [--sandbox rootdir] [--arg cabal-arg]"
