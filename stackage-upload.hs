{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad                         (filterM, when)
import qualified Data.ByteString.Char8                 as S8
import qualified Data.ByteString.Lazy                  as L
import qualified Data.ByteString.Lazy.Char8            as L8
import           Data.List                             (isInfixOf, isPrefixOf,
                                                        sort)
import           Network.HTTP.Client
import           Network.HTTP.Client.MultipartFormData
import           System.Directory                      (doesDirectoryExist,
                                                        getDirectoryContents)
import           System.Environment                    (getArgs, getEnv,
                                                        getProgName)
import           System.Exit                           (exitFailure)
import           System.Exit                           (ExitCode (ExitSuccess))
import           System.FilePath                       (takeDirectory, (</>))
import           System.Process                        (createProcess, cwd,
                                                        proc, waitForProcess)

main :: IO ()
main = withManager defaultManagerSettings $ \m -> do
    args <- getArgs
    token <- readFile "/auth-token"
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

    let uploadDocs = "exclusive" `isInfixOf` alias
        uploadHackageDistro = alias == "unstable-ghc78-exclusive"

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
            , redirectCount = 0
            , checkStatus = \_ _ _ -> Nothing
            }
    res <- httpLbs req3 m

    snapid <-
        case lookup "x-stackage-ident" $ responseHeaders res of
            Just snapid -> do
                putStrLn $ "New ident: " ++ S8.unpack snapid
                return snapid
            Nothing -> error $ "An error occurred: " ++ show res

    when uploadDocs $ do
        putStrLn "Generating index file"
        let root = takeDirectory filepath </> "haddock"
        contents <- getDirectoryContents root
        dirs <- filterM (\n -> doesDirectoryExist $ root </> n)
              $ filter (not . ("." `isPrefixOf`))
              $ sort contents
        writeFile (root </> "index.html") $ mkIndex (S8.unpack snapid) dirs
        writeFile (root </> "style.css") styleCss

        putStrLn "Creating tarball"
        (Nothing, Nothing, Nothing, ph) <- createProcess
            (proc "tar" $ "cJf" : "haddock.tar.xz" : "index.html" : "style.css" : dirs)
                { cwd = Just root
                }
        ec <- waitForProcess ph
        if ec == ExitSuccess
            then putStrLn "Haddock tarball generated"
            else error "Error generating Haddock tarball"

        putStrLn "Uploading Haddocks"

        req1 <- parseUrl $ "http://www.stackage.org/upload-haddock/"
                        ++ S8.unpack snapid
        let formData =
                [ partFileSource "tarball" $ root </> "haddock.tar.xz"
                ]
        req2 <- formDataBody formData req1
        let req3 = req2
                { method = "PUT"
                , requestHeaders =
                    [ ("Authorization", S8.pack token)
                    , ("Accept", "application/json")
                    ] ++ requestHeaders req2
                , redirectCount = 0
                , checkStatus = \_ _ _ -> Nothing
                }
        httpLbs req3 m >>= print

    when uploadHackageDistro $ do
        lbs <- L.readFile $ takeDirectory filepath </> "build-plan.csv"
        let req = "http://hackage.haskell.org/distro/Stackage/packages.csv"
                { requestHeaders = [("Content-Type", "text/csv")]
                , requestBody = RequestBodyLBS $ L.intercalate "\n" $ L8.lines lbs
                , checkStatus = \_ _ _ -> Nothing
                , method = "PUT"
                }
        httpLbs req m >>= print

mkIndex :: String -> [String] -> String
mkIndex snapid dirs = concat
    [ "<!DOCTYPE html>\n<html lang='en'><head><title>Haddocks index</title>"
    , "<link rel='stylesheet' href='https://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css'>"
    , "<link rel='stylesheet' href='style.css'>"
    , "<link rel='shortcut icon' href='http://www.stackage.org/static/img/favicon.ico' />"
    , "</head>"
    , "<body><div class='container'>"
    , "<div class='row'><div class='span12 col-md-12'>"
    , "<h1>Haddock documentation index</h1>"
    , "<p class='return'><a href=\"http://www.stackage.org/stackage/"
    , snapid
    , "\">Return to snapshot</a></p><ul>"
    , concatMap toLI dirs
    , "</ul></div></div></div></body></html>"
    ]
  where
    toLI name = concat
        [ "<li><a href='"
        , name
        , "/index.html'>"
        , name
        , "</a></li>"
        ]

styleCss :: String
styleCss = concat
    [ "@media (min-width: 530px) {"
    , "ul { -webkit-column-count: 2; -moz-column-count: 2; column-count: 2 }"
    , "}"
    , "@media (min-width: 760px) {"
    , "ul { -webkit-column-count: 3; -moz-column-count: 3; column-count: 3 }"
    , "}"
    , "ul {"
    , "  margin-left: 0;"
    , "  padding-left: 0;"
    , "  list-style-type: none;"
    , "}"
    , "body {"
    , "  background: #f0f0f0;"
    , "  font-family: 'Lato', sans-serif;"
    , "  text-shadow: 1px 1px 1px #ffffff;"
    , "  font-size: 20px;"
    , "  line-height: 30px;"
    , "  padding-bottom: 5em;"
    , "}"
    , "h1 {"
    , "  font-weight: normal;"
    , "  color: #06537d;"
    , "  font-size: 45px;"
    , "}"
    , ".return a {"
    , "  color: #06537d;"
    , "  font-style: italic;"
    , "}"
    , ".return {"
    , "  margin-bottom: 1em;"
    , "}"]
