{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Stackage2.CompleteBuild
    ( BuildType (..)
    , BumpType (..)
    , completeBuild
    ) where
import Data.Default.Class         (def)
import Data.Semigroup             (Max (..), Option (..))
import Data.Text.Read             (decimal)
import Data.Time
import Data.Yaml                  (decodeFileEither, encodeFile)
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

data BuildType = Nightly | LTS BumpType
    deriving (Show, Read, Eq, Ord)

data BumpType = Major | Minor
    deriving (Show, Read, Eq, Ord)

data Settings = Settings
    { plan      :: BuildPlan
    , planFile  :: FilePath
    , buildDir  :: FilePath
    , logDir    :: FilePath
    , title     :: Text -> Text -- ^ GHC version -> title
    , slug      :: Text
    , setArgs   :: Text -> UploadBundle -> UploadBundle
    , postBuild :: IO ()
    }

getSettings :: BuildType -> IO Settings
getSettings Nightly = do
    day <- tshow . utctDay <$> getCurrentTime
    let slug' = "nightly-" ++ day
    plan' <- defaultBuildConstraints >>= newBuildPlan
    return Settings
        { planFile = fpFromText ("nightly-" ++ day) <.> "yaml"
        , buildDir = fpFromText $ "builds/stackage-nightly-" ++ day
        , logDir = fpFromText $ "logs/stackage-nightly-" ++ day
        , title = \ghcVer -> concat
            [ "Stackage Nightly "
            , day
            , ", GHC "
            , ghcVer
            ]
        , slug = slug'
        , setArgs = \ghcVer ub -> ub { ubNightly = Just ghcVer }
        , plan = plan'
        , postBuild = return ()
        }
getSettings (LTS bumpType) = do
    Option mlts <- fmap (fmap getMax) $ runResourceT
        $ sourceDirectory "."
       $$ foldMapC (Option . fmap Max . parseLTSVer . filename)

    (new, plan') <- case bumpType of
        Major -> do
            let new =
                    case mlts of
                        Nothing -> LTSVer 0 0
                        Just (LTSVer x _) -> LTSVer (x + 1) 0
            plan' <- defaultBuildConstraints >>= newBuildPlan
            return (new, plan')
        Minor -> do
            old <- maybe (error "No LTS plans found in current directory") return mlts
            oldplan <- decodeFileEither (fpToString $ renderLTSVer old)
                   >>= either throwM return
            let new = incrLTSVer old
            plan' <- updateBuildPlan oldplan
            return (new, plan')

    let newfile = renderLTSVer new

    return Settings
        { planFile = newfile
        , buildDir = fpFromText $ "builds/stackage-lts-" ++ tshow new
        , logDir = fpFromText $ "logs/stackage-lts-" ++ tshow new
        , title = \ghcVer -> concat
            [ "LTS Haskell "
            , tshow new
            , ", GHC "
            , ghcVer
            ]
        , slug = "lts-" ++ tshow new
        , setArgs = \_ ub -> ub { ubLTS = Just $ tshow new }
        , plan = plan'
        , postBuild = do
            let git args = withCheckedProcess
                    (proc "git" args) $ \ClosedStream Inherited Inherited ->
                        return ()
            putStrLn "Committing new LTS file to Git"
            git ["add", fpToString newfile]
            git ["commit", "-m", "Added new LTS release: " ++ show new]
            putStrLn "Pushing to Git repository"
            git ["push"]
        }

data LTSVer = LTSVer !Int !Int
    deriving (Eq, Ord)
instance Show LTSVer where
    show (LTSVer x y) = concat [show x, ".", show y]
incrLTSVer :: LTSVer -> LTSVer
incrLTSVer (LTSVer x y) = LTSVer x (y + 1)

parseLTSVer :: FilePath -> Maybe LTSVer
parseLTSVer fp = do
    w <- stripPrefix "lts-" $ fpToText fp
    x <- stripSuffix ".yaml" w
    Right (major, y) <- Just $ decimal x
    z <- stripPrefix "." y
    Right (minor, "") <- Just $ decimal z
    return $ LTSVer major minor
renderLTSVer :: LTSVer -> FilePath
renderLTSVer lts = fpFromText $ concat
    [ "lts-"
    , tshow lts
    , ".yaml"
    ]

completeBuild :: BuildType -> IO ()
completeBuild buildType = withManager defaultManagerSettings $ \man -> do
    hSetBuffering stdout LineBuffering

    putStrLn $ "Loading settings for: " ++ tshow buildType
    Settings {..} <- getSettings buildType

    putStrLn $ "Writing build plan to: " ++ fpToText planFile
    encodeFile (fpToString planFile) plan

    putStrLn "Checking build plan"
    checkBuildPlan plan

    putStrLn "Performing build"
    let pb = PerformBuild
            { pbPlan = plan
            , pbInstallDest = buildDir
            , pbLogDir = logDir
            , pbLog = hPut stdout
            , pbJobs = 8
            }
    performBuild pb >>= mapM_ putStrLn

    putStrLn "Uploading bundle to Stackage Server"
    token <- readFile "/auth-token"
    now <- epochTime
    let ghcVer = display $ siGhcVersion $ bpSystemInfo plan
    ident <- flip uploadBundle man $ setArgs ghcVer def
        { ubContents = serverBundle now (title ghcVer) slug plan
        , ubAuthToken = decodeUtf8 token
        }
    putStrLn $ "New ident: " ++ unSnapshotIdent ident

    putStrLn "Uploading docs to Stackage Server"
    res1 <- uploadDocs UploadDocs
        { udServer = def
        , udAuthToken = decodeUtf8 token
        , udDocs = pbDocDir pb
        , udSnapshot = ident
        } man
    putStrLn $ "Doc upload response: " ++ tshow res1

    ecreds <- tryIO $ readFile "/hackage-creds"
    case map encodeUtf8 $ words $ decodeUtf8 $ either (const "") id ecreds of
        [username, password] -> do
            putStrLn "Uploading as Hackage distro"
            res2 <- uploadHackageDistro plan username password man
            putStrLn $ "Distro upload response: " ++ tshow res2
        _ -> putStrLn "No creds found, skipping Hackage distro upload"

    postBuild
