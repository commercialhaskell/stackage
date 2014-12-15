import Stackage.CompleteBuild
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [x] | Just y <- lookup x m -> completeBuild y
        _ -> error $ "Expected one argument, one of: " ++ unwords (map fst m)
  where
    m =
        [ ("nightly", Nightly)
        , ("lts-major", LTS Major)
        , ("lts-minor", LTS Minor)
        ]
