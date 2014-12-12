{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Stackage2.CompleteBuild
    ( BuildType (..)
    , completeBuild
    ) where
import Data.Default.Class         (def)
import Data.Time
import Data.Yaml                  (encodeFile)
import Network.HTTP.Client
import Stackage2.BuildConstraints
import Stackage2.BuildPlan
import Stackage2.CheckBuildPlan
import Stackage2.PerformBuild
import Stackage2.Prelude
import Stackage2.ServerBundle
import Stackage2.UpdateBuildPlan
import Stackage2.Upload
import System.IO                  (BufferMode (LineBuffering), hSetBuffering)

data BuildType = Nightly | LTS

data Settings = Settings
    { plan :: BuildPlan
    , planFile :: FilePath
    , buildDir :: FilePath
    , title :: Text -> Text -- ^ GHC version -> title
    , slug :: Text
    , setArgs :: Text -> UploadBundle -> UploadBundle
    }

getSettings :: BuildType -> IO Settings
getSettings Nightly = do
    day <- tshow . utctDay <$> getCurrentTime
    let slug' = "nightly-" ++ day
    plan' <- defaultBuildConstraints >>= newBuildPlan
    return Settings
        { planFile = fpFromText ("nightly-" ++ day) <.> "yaml"
        , buildDir = fpFromText $ "/tmp/stackage-nightly-" ++ day
        , title = \ghcVer -> concat
            [ "Stackage Nightly "
            , day
            , ", GHC "
            , ghcVer
            ]
        , slug = slug'
        , setArgs = \ghcVer ub -> ub { ubNightly = Just ghcVer }
        , plan = plan'
        }

completeBuild :: BuildType -> IO ()
completeBuild buildType = withManager defaultManagerSettings $ \man -> do
    hSetBuffering stdout LineBuffering

    Settings {..} <- getSettings buildType
    encodeFile (fpToString planFile) plan
    checkBuildPlan plan
    let pb = PerformBuild
            { pbPlan = plan
            , pbInstallDest = buildDir
            , pbLogDir = buildDir </> "logs"
            , pbLog = hPut stdout
            , pbJobs = 8
            }
    performBuild pb

    token <- readFile "/auth-token"
    now <- epochTime
    let ghcVer = display $ siGhcVersion $ bpSystemInfo plan
    ident <- flip uploadBundle man $ setArgs ghcVer def
        { ubContents = serverBundle now (title ghcVer) slug plan
        , ubAuthToken = decodeUtf8 token
        , ubAlias = Just slug
        }

    uploadDocs UploadDocs
        { udServer = def
        , udAuthToken = decodeUtf8 token
        , udDocs = pbDocDir pb
        , udSnapshot = ident
        } man >>= print

    creds <- readFile "/hackage-creds"
    case map encodeUtf8 $ words $ decodeUtf8 creds of
        [username, password] ->
            uploadHackageDistro plan username password man >>= print
        _ -> return ()
