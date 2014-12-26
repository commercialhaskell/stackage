-- | Upload to Stackage and Hackage
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
module Stackage.Upload
    ( UploadBundle (..)
    , SnapshotIdent (..)
    , uploadBundle
    , UploadDocs (..)
    , uploadDocs
    , uploadHackageDistro
    , UploadDocMap (..)
    , uploadDocMap
    , uploadBundleV2
    , UploadBundleV2 (..)
    ) where

import Control.Monad.Writer.Strict           (execWriter, tell)
import Data.Default.Class                    (Default (..))
import Data.Function                         (fix)
import Filesystem                            (isDirectory, isFile)
import Network.HTTP.Client
import qualified Network.HTTP.Client.Conduit as HCC
import Network.HTTP.Client.MultipartFormData
import Stackage.BuildPlan                    (BuildPlan)
import Stackage.Prelude
import Stackage.ServerBundle                 (bpAllPackages, docsListing, writeIndexStyle)
import System.IO.Temp                        (withSystemTempFile)
import qualified System.IO as IO
import qualified Data.Yaml as Y

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

uploadBundle :: UploadBundle -> Manager -> IO (SnapshotIdent, Maybe Text)
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
        Just snapid -> return
            ( SnapshotIdent $ decodeUtf8 snapid
            , decodeUtf8 <$> lookup "location" (responseHeaders res)
            )
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

uploadDocs :: UploadDocs -> Manager -> IO (Response LByteString)
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
        dirs <- writeIndexStyle (Just $ unSnapshotIdent ident) fp0
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
    { udmServer    :: StackageServer
    , udmAuthToken :: Text
    , udmSnapshot  :: SnapshotIdent
    , udmDocDir    :: FilePath
    , udmPlan      :: BuildPlan
    }

uploadDocMap :: UploadDocMap -> Manager -> IO (Response LByteString)
uploadDocMap UploadDocMap {..} man = do
    docmap <- docsListing udmPlan udmDocDir
    req1 <- parseUrl $ unpack $ unStackageServer udmServer ++ "/upload-doc-map"
    req2 <- formDataBody (formData $ Y.encode docmap) req1
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

data UploadBundleV2 = UploadBundleV2
    { ub2Server :: StackageServer
    , ub2AuthToken :: Text
    , ub2Bundle :: FilePath
    }

uploadBundleV2 :: UploadBundleV2 -> Manager -> IO Text
uploadBundleV2 UploadBundleV2 {..} man = IO.withBinaryFile (fpToString ub2Bundle) IO.ReadMode $ \h -> do
    size <- IO.hFileSize h
    req1 <- parseUrl $ unpack $ unStackageServer ub2Server ++ "/upload2"
    let req2 = req1
            { method = "PUT"
            , requestHeaders =
                [ ("Authorization", encodeUtf8 ub2AuthToken)
                , ("Accept", "application/json")
                , ("Content-Type", "application/x-tar")
                ]
            , requestBody = HCC.requestBodySource (fromIntegral size)
                          $ sourceHandle h $= printProgress size
            }
        sink = decodeUtf8C =$ fix (\loop -> do
            mx <- peekC
            case mx of
                Nothing -> error $ "uploadBundleV2: premature end of stream"
                Just _ -> do
                    l <- lineC $ takeCE 4096 =$ foldC
                    let (cmd, msg') = break (== ':') l
                        msg = dropWhile (== ' ') $ dropWhile (== ':') msg'
                    case cmd of
                        "CONT" -> do
                            putStrLn msg
                            loop
                        "FAILURE" -> error $ "uploadBundleV2 failed: " ++ unpack msg
                        "SUCCESS" -> return msg
                        _ -> error $ "uploadBundleV2: unknown command " ++ unpack cmd
            )
    withResponse req2 man $ \res -> HCC.bodyReaderSource (responseBody res) $$ sink
  where
    printProgress total =
        loop 0 0
      where
        loop sent lastPercent =
            await >>= maybe (putStrLn "Upload complete") go
          where
            go bs = do
                yield bs
                let sent' = sent + fromIntegral (length bs)
                    percent = sent' * 100 `div` total
                when (percent /= lastPercent)
                    $ putStrLn $ "Upload progress: " ++ tshow percent ++ "%"
                loop sent' percent
