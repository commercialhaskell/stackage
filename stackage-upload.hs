{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Char8                 as S8
import           Network.HTTP.Client
import           Network.HTTP.Client.MultipartFormData
import           System.Environment                    (getArgs, getEnv,
                                                        getProgName)
import           System.Exit                           (exitFailure)

main :: IO ()
main = withManager defaultManagerSettings $ \m -> do
    args <- getArgs
    token <- getEnv "STACKAGE_AUTH_TOKEN"
    (filepath, alias) <-
        case args of
            [x, y] -> return (x, y)
            _ -> do
                pn <- getProgName
                putStrLn $ concat
                    [ "Usage: "
                    , pn
                    , " <filepath> <alias name>"
                    ]
                exitFailure

    putStrLn $ concat
        [ "Uploading "
        , filepath
        , " as "
        , alias
        ]

    req1 <- parseUrl "http://www.stackage.org/upload"
    let formData =
            [ partBS "alias" $ S8.pack alias
            , partFileSource "stackage" filepath
            ]
    req2 <- formDataBody formData req1
    let req3 = req2
            { method = "PUT"
            , requestHeaders =
                [ ("Authorization", S8.pack token)
                , ("Accept", "application/json")
                ] ++ requestHeaders req2
            }
    httpLbs req3 m >>= print
