{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Stackage.CompleteBuild
    ( BuildType (..)
    , BumpType (..)
    , BuildFlags (..)
    , completeBuild
    , justCheck
    , justUploadNightly
    ) where

import Control.Concurrent        (threadDelay)
import Control.Concurrent.Async  (withAsync)
import Data.Default.Class        (def)
import Data.Semigroup            (Max (..), Option (..))
import Data.Text.Read            (decimal)
import Data.Time
import Data.Yaml                 (decodeFileEither, encodeFile)
import Network.HTTP.Client
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import Stackage.BuildConstraints
import Stackage.BuildPlan
import Stackage.CheckBuildPlan
import Stackage.PerformBuild
import Stackage.Prelude
import Stackage.ServerBundle
import Stackage.UpdateBuildPlan
import Stackage.Upload
import System.Environment        (lookupEnv)
import System.IO                 (BufferMode (LineBuffering), hSetBuffering)

-- | Flags passed in from the command line.
data BuildFlags = BuildFlags
    { bfEnableTests      :: !Bool
    , bfEnableHaddock    :: !Bool
    , bfDoUpload         :: !Bool
    , bfEnableLibProfile :: !Bool
    , bfVerbose          :: !Bool
    , bfSkipCheck        :: !Bool
    } deriving (Show)

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
    , distroName :: Text -- ^ distro name on Hackage
    }

nightlyPlanFile :: Text -- ^ day
                -> FilePath
nightlyPlanFile day = fpFromText ("nightly-" ++ day) <.> "yaml"

nightlySettings :: Text -- ^ day
                -> BuildPlan
                -> Settings
nightlySettings day plan' = Settings
    { planFile = nightlyPlanFile day
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
    , distroName = "Stackage"
    }
  where
    slug' = "nightly-" ++ day

getSettings :: Manager -> BuildType -> IO Settings
getSettings man Nightly = do
    day <- tshow . utctDay <$> getCurrentTime
    bc <- defaultBuildConstraints man
    pkgs <- getLatestAllowedPlans bc
    plan' <- newBuildPlan pkgs bc
    return $ nightlySettings day plan'
getSettings man (LTS bumpType) = do
    Option mlts <- fmap (fmap getMax) $ runResourceT
        $ sourceDirectory "."
       $$ foldMapC (Option . fmap Max . parseLTSVer . filename)

    (new, plan') <- case bumpType of
        Major -> do
            let new =
                    case mlts of
                        Nothing -> LTSVer 0 0
                        Just (LTSVer x _) -> LTSVer (x + 1) 0
            bc <- defaultBuildConstraints man
            pkgs <- getLatestAllowedPlans bc
            plan' <- newBuildPlan pkgs bc
            return (new, plan')
        Minor -> do
            old <- maybe (error "No LTS plans found in current directory") return mlts
            oldplan <- decodeFileEither (fpToString $ renderLTSVer old)
                   >>= either throwM return
            let new = incrLTSVer old
            let bc = updateBuildConstraints oldplan
            pkgs <- getLatestAllowedPlans bc
            plan' <- newBuildPlan pkgs bc
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
        , distroName = "LTSHaskell"
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

-- | Just print a message saying "still alive" every minute, to appease Travis.
stillAlive :: IO () -> IO ()
stillAlive inner =
    withAsync (printer 1) $ const inner
  where
    printer i = forever $ do
        threadDelay 60000000
        putStrLn $ "Still alive: " ++ tshow i
        printer $! i + 1

-- | Generate and check a new build plan, but do not execute it.
--
-- Since 0.3.1
justCheck :: IO ()
justCheck = stillAlive $ withManager tlsManagerSettings $ \man -> do
    putStrLn "Loading build constraints"
    bc <- defaultBuildConstraints man

    putStrLn "Creating build plan"
    plans <- getLatestAllowedPlans bc
    plan <- newBuildPlan plans bc

    putStrLn $ "Writing build plan to check-plan.yaml"
    encodeFile "check-plan.yaml" plan

    putStrLn "Checking plan"
    checkBuildPlan plan

    putStrLn "Plan seems valid!"

getPerformBuild :: BuildFlags -> Settings -> PerformBuild
getPerformBuild buildFlags Settings {..} = PerformBuild
    { pbPlan = plan
    , pbInstallDest = buildDir
    , pbLogDir = logDir
    , pbLog = hPut stdout
    , pbJobs = 8
    , pbGlobalInstall = False
    , pbEnableTests = bfEnableTests buildFlags
    , pbEnableHaddock = bfEnableHaddock buildFlags
    , pbEnableLibProfiling = bfEnableLibProfile buildFlags
    , pbVerbose = bfVerbose buildFlags
    , pbAllowNewer = bfSkipCheck buildFlags
    }

-- | Make a complete plan, build, test and upload bundle, docs and
-- distro.
completeBuild :: BuildType -> BuildFlags -> IO ()
completeBuild buildType buildFlags = withManager tlsManagerSettings $ \man -> do
    hSetBuffering stdout LineBuffering

    putStrLn $ "Loading settings for: " ++ tshow buildType
    settings@Settings {..} <- getSettings man buildType

    putStrLn $ "Writing build plan to: " ++ fpToText planFile
    encodeFile (fpToString planFile) plan

    if bfSkipCheck buildFlags
        then putStrLn "Skipping build plan check"
        else do
            putStrLn "Checking build plan"
            checkBuildPlan plan

    putStrLn "Performing build"
    performBuild (getPerformBuild buildFlags settings) >>= mapM_ putStrLn

    when (bfDoUpload buildFlags) $
        finallyUpload settings man

justUploadNightly
    :: Text -- ^ nightly date
    -> IO ()
justUploadNightly day = do
    plan <- decodeFileEither (fpToString $ nightlyPlanFile day)
        >>= either throwM return
    withManager tlsManagerSettings $ finallyUpload $ nightlySettings day plan

-- | The final part of the complete build process: uploading a bundle,
-- docs and a distro to hackage.
finallyUpload :: Settings -> Manager -> IO ()
finallyUpload settings@Settings{..} man = do
    putStrLn "Uploading bundle to Stackage Server"

    mtoken <- lookupEnv "STACKAGE_AUTH_TOKEN"
    token <-
        case mtoken of
            Nothing -> decodeUtf8 <$> readFile "/auth-token"
            Just token -> return $ pack token

    now <- epochTime
    let ghcVer = display $ siGhcVersion $ bpSystemInfo plan
    (ident, mloc) <- flip uploadBundle man $ setArgs ghcVer def
        { ubContents = serverBundle now (title ghcVer) slug plan
        , ubAuthToken = token
        }
    putStrLn $ "New ident: " ++ unSnapshotIdent ident
    forM_ mloc $ \loc ->
        putStrLn $ "Track progress at: " ++ loc

    postBuild `catchAny` print

    putStrLn "Uploading docs to Stackage Server"
    res1 <- uploadDocs UploadDocs
        { udServer = def
        , udAuthToken = token
        , udDocs = pbDocDir pb
        , udSnapshot = ident
        } man
    putStrLn $ "Doc upload response: " ++ tshow res1

    ecreds <- tryIO $ readFile "/hackage-creds"
    case map encodeUtf8 $ words $ decodeUtf8 $ either (const "") id ecreds of
        [username, password] -> do
            putStrLn "Uploading as Hackage distro"
            res2 <- uploadHackageDistroNamed distroName plan username password man
            putStrLn $ "Distro upload response: " ++ tshow res2
        _ -> putStrLn "No creds found, skipping Hackage distro upload"

    putStrLn "Uploading doc map"
    uploadDocMap UploadDocMap
        { udmServer = def
        , udmAuthToken = token
        , udmSnapshot = ident
        , udmDocDir = pbDocDir pb
        , udmPlan = plan
        } man >>= print
  where
    pb = getPerformBuild (error "finallyUpload.buildFlags") settings
