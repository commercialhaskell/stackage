-- | Upload to Stackage and Hackage
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
module Stackage2.Upload
    ( UploadBundle (..)
    , SnapshotIdent (..)
    , uploadBundle
    , UploadDocs (..)
    , uploadDocs
    , uploadHackageDistro
    , UploadDocMap (..)
    , uploadDocMap
    ) where

import Control.Monad.Writer.Strict           (execWriter, tell)
import Data.Default.Class                    (Default (..))
import Filesystem                            (isDirectory, isFile)
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData
import Stackage2.BuildPlan                   (BuildPlan)
import Stackage2.Prelude
import Stackage2.ServerBundle                (bpAllPackages, docsListing)
import System.IO.Temp                        (withSystemTempFile)

newtype StackageServer = StackageServer { unStackageServer :: Text }
    deriving (Show, Eq, Ord, Hashable, IsString)
instance Default StackageServer where
    def = "http://www.stackage.org"

data UploadBundle = UploadBundle
    { ubServer    :: StackageServer
    , ubContents  :: LByteString
    , ubAlias     :: Maybe Text
    , ubNightly   :: Maybe Text -- ^ should be GHC version
    , ubLTS       :: Maybe Text -- ^ e.g. 2.3
    , ubAuthToken :: Text
    }
instance Default UploadBundle where
    def = UploadBundle
        { ubServer = def
        , ubContents = mempty
        , ubAlias = Nothing
        , ubNightly = Nothing
        , ubLTS = Nothing
        , ubAuthToken = "no-auth-token-provided"
        }

newtype SnapshotIdent = SnapshotIdent { unSnapshotIdent :: Text }
    deriving (Show, Eq, Ord, Hashable, IsString)

uploadBundle :: UploadBundle -> Manager -> IO SnapshotIdent
uploadBundle UploadBundle {..} man = do
    req1 <- parseUrl $ unpack $ unStackageServer ubServer ++ "/upload"
    req2 <- formDataBody formData req1
    let req3 = req2
            { method = "PUT"
            , requestHeaders =
                [ ("Authorization", encodeUtf8 ubAuthToken)
                , ("Accept", "application/json")
                ] ++ requestHeaders req2
            , redirectCount = 0
            , checkStatus = \_ _ _ -> Nothing
            , responseTimeout = Just 300000000
            }
    res <- httpLbs req3 man
    case lookup "x-stackage-ident" $ responseHeaders res of
        Just snapid -> return $ SnapshotIdent $ decodeUtf8 snapid
        Nothing -> error $ "An error occurred: " ++ show res
  where
    params = mapMaybe (\(x, y) -> (x, ) <$> y)
        [ ("alias", ubAlias)
        , ("nightly", ubNightly)
        , ("lts", ubLTS)
        ]
    formData = ($ []) $ execWriter $ do
        forM_ params $ \(key, value) ->
            tell' $ partBS key $ encodeUtf8 value
        tell' $ partFileRequestBody "stackage" "stackage"
            $ RequestBodyLBS ubContents

    tell' x = tell (x:)

data UploadDocs = UploadDocs
    { udServer    :: StackageServer
    , udDocs      :: FilePath -- ^ may be a directory or a tarball
    , udAuthToken :: Text
    , udSnapshot  :: SnapshotIdent
    }

uploadDocs (UploadDocs (StackageServer host) fp0 token ident) man = do
    fe <- isFile fp0
    if fe
        then uploadDocsFile $ fpToString fp0
        else do
            de <- isDirectory fp0
            if de
                then uploadDocsDir
                else error $ "Path not found: " ++ fpToString fp0
  where
    uploadDocsDir = withSystemTempFile "haddocks.tar.xz" $ \fp h -> do
        hClose h
        dirs <- fmap sort
              $ runResourceT
              $ sourceDirectory fp0
             $$ filterMC (liftIO . isDirectory)
             =$ mapC (fpToString . filename)
             =$ sinkList
        writeFile (fp0 </> "index.html") $ mkIndex
            (unpack $ unSnapshotIdent ident)
            dirs
        writeFile (fp0 </> "style.css") styleCss
        -- FIXME write index.html, style.css
        let cp = (proc "tar" $ "cJf" : fp : "index.html" : "style.css" : dirs)
                { cwd = Just $ fpToString fp0
                }
        withCheckedProcess cp $ \Inherited Inherited Inherited -> return ()
        uploadDocsFile fp
    uploadDocsFile fp = do
        req1 <- parseUrl $ unpack $ concat
            [ host
            , "/upload-haddock/"
            , unSnapshotIdent ident
            ]
        let formData =
                [ partFileSource "tarball" fp
                ]
        req2 <- formDataBody formData req1
        let req3 = req2
                { method = "PUT"
                , requestHeaders =
                    [ ("Authorization", encodeUtf8 token)
                    , ("Accept", "application/json")
                    ] ++ requestHeaders req2
                , redirectCount = 0
                , checkStatus = \_ _ _ -> Nothing
                , responseTimeout = Just 300000000
                }
        httpLbs req3 man

uploadHackageDistro :: BuildPlan
                    -> ByteString -- ^ Hackage username
                    -> ByteString -- ^ Hackage password
                    -> Manager
                    -> IO (Response LByteString)
uploadHackageDistro bp username password =
    httpLbs (applyBasicAuth username password req)
  where
    csv = encodeUtf8
        $ builderToLazy
        $ mconcat
        $ intersperse "\n"
        $ map go
        $ mapToList
        $ bpAllPackages bp
    go (name, version) =
        "\"" ++
        (toBuilder $ display name) ++
        "\",\"" ++
        (toBuilder $ display version) ++
        "\",\"http://www.stackage.org/package/" ++
        (toBuilder $ display name) ++
        "\""

    req = "http://hackage.haskell.org/distro/Stackage/packages.csv"
        { requestHeaders = [("Content-Type", "text/csv")]
        , requestBody = RequestBodyLBS csv
        , checkStatus = \_ _ _ -> Nothing
        , method = "PUT"
        }

data UploadDocMap = UploadDocMap
    { udmServer :: StackageServer
    , udmAuthToken :: Text
    , udmSnapshot :: SnapshotIdent
    , udmDocDir :: FilePath
    , udmPlan :: BuildPlan
    }

uploadDocMap UploadDocMap {..} man = do
    docmap <- docsListing udmPlan udmDocDir
    req1 <- parseUrl $ unpack $ unStackageServer udmServer ++ "/upload-doc-map"
    req2 <- formDataBody (formData docmap) req1
    let req3 = req2
            { method = "PUT"
            , requestHeaders =
                [ ("Authorization", encodeUtf8 udmAuthToken)
                , ("Accept", "application/json")
                ] ++ requestHeaders req2
            , redirectCount = 0
            , checkStatus = \_ _ _ -> Nothing
            , responseTimeout = Just 300000000
            }
    httpLbs req3 man
  where
    formData docmap =
        [ partBS "snapshot" (encodeUtf8 $ unSnapshotIdent udmSnapshot)
        , partFileRequestBody "docmap" "docmap" $ RequestBodyBS docmap
        ]

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
