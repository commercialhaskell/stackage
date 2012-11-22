import           Stackage.Build     (build)
import           Stackage.Init      (stackageInit)
import           System.Environment (getArgs, getProgName)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["build"] -> build
        ["init"] -> stackageInit
        ["update"] -> stackageInit >> error "FIXME update"
        _ -> do
            pn <- getProgName
            putStrLn $ "Usage: " ++ pn ++ " <command>"
            putStrLn "Available commands:"
            putStrLn "    update     Download updated Stackage databases. Automatically calls init."
            putStrLn "    init       Initialize your cabal file to use Stackage"
            putStrLn "    build      Build the package databases (maintainers only)"
