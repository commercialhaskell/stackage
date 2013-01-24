{-# LANGUAGE RecordWildCards #-}
import           Stackage.Types
import           Stackage.Build     (build, defaultBuildSettings)
import           Stackage.Init      (stackageInit)
import           Stackage.Util      (allowPermissive)
import           System.Environment (getArgs, getProgName)
import           Data.Set           (fromList)
import           System.IO          (hFlush, stdout)

data BuildArgs = BuildArgs
    { excluded :: [String]
    , noPlatform :: Bool
    , onlyPermissive :: Bool
    , allowed :: [String]
    }

parseBuildArgs :: [String] -> IO BuildArgs
parseBuildArgs =
    loop BuildArgs
        { excluded = []
        , noPlatform = False
        , onlyPermissive = False
        , allowed = []
        }
  where
    loop x [] = return x
    loop x ("--exclude":y:rest) = loop x { excluded = y : excluded x } rest
    loop x ("--no-platform":rest) = loop x { noPlatform = True } rest
    loop x ("--only-permissive":rest) = loop x { onlyPermissive = True } rest
    loop x ("--allow":y:rest) = loop x { allowed = y : allowed x } rest
    loop _ (y:_) = error $ "Did not understand argument: " ++ y

main :: IO ()
main = do
    args <- getArgs
    case args of
        "build":rest -> do
            BuildArgs {..} <- parseBuildArgs rest
            build defaultBuildSettings
                { excludedPackages = fromList $ map PackageName excluded
                , requireHaskellPlatform = not noPlatform
                , allowedPackage =
                    if onlyPermissive
                        then allowPermissive allowed
                        else const $ Right ()
                }
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
            putStrLn "    update              Download updated Stackage databases. Automatically calls init."
            putStrLn "    init                Initialize your cabal file to use Stackage"
            putStrLn "    build [--no-clean] [--no-platform] [--exclude package...] [--only-permissive] [--allow package]"
            putStrLn "                        Build the package databases (maintainers only)"
