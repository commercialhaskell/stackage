import Stackage.CompleteBuild
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [x] | Just y <- lookup x m -> y
        _ -> error $ "Expected one argument, one of: " ++ unwords (map fst m)
  where
    m =
        [ ("nightly", completeBuild Nightly)
        , ("lts-major", completeBuild $ LTS Major)
        , ("lts-minor", completeBuild $ LTS Minor)
        , ("check", justCheck)
        ]
