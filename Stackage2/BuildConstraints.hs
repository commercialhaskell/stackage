{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
-- | The constraints on package selection for a new build plan.
module Stackage2.BuildConstraints
    ( BuildConstraints (..)
    , PackageConstraints (..)
    , TestState (..)
    , SystemInfo (..)
    , defaultBuildConstraints
    ) where

import           Data.Aeson
import qualified Data.Map               as Map
import           Distribution.System    (Arch, OS)
import qualified Distribution.System
import           Distribution.Version   (anyVersion)
import qualified Stackage.Config        as Old
import qualified Stackage.Select        as Old
import qualified Stackage.Types         as Old
import           Stackage2.CorePackages
import           Stackage2.Prelude

data TestState = ExpectSuccess
               | ExpectFailure
               | Don'tBuild -- ^ when the test suite will pull in things we don't want
    deriving (Show, Eq, Ord, Bounded, Enum)

testStateToText :: TestState -> Text
testStateToText ExpectSuccess = "expect-success"
testStateToText ExpectFailure = "expect-failure"
testStateToText Don'tBuild    = "do-not-build"

instance ToJSON TestState where
    toJSON = toJSON . testStateToText
instance FromJSON TestState where
    parseJSON = withText "TestState" $ \t ->
        case lookup t states of
            Nothing -> fail $ "Invalid state: " ++ unpack t
            Just v -> return v
      where
        states = asHashMap $ mapFromList
               $ map (\x -> (testStateToText x, x)) [minBound..maxBound]

data SystemInfo = SystemInfo
    { siGhcVersion      :: Version
    , siOS              :: OS
    , siArch            :: Arch
    , siCorePackages    :: Map PackageName Version
    , siCoreExecutables :: Set ExeName
    }
    deriving (Show, Eq, Ord)
instance ToJSON SystemInfo where
    toJSON SystemInfo {..} = object
        [ "ghc-version" .= display siGhcVersion
        , "os" .= display siOS
        , "arch" .= display siArch
        , "core-packages" .= Map.mapKeysWith const unPackageName (map display siCorePackages)
        , "core-executables" .= siCoreExecutables
        ]
instance FromJSON SystemInfo where
    parseJSON = withObject "SystemInfo" $ \o -> do
        let helper name = (o .: name) >>= either (fail . show) return . simpleParse
        siGhcVersion <- helper "ghc-version"
        siOS <- helper "os"
        siArch <- helper "arch"
        siCorePackages <- (o .: "core-packages") >>= goPackages
        siCoreExecutables <- o .: "core-executables"
        return SystemInfo {..}
      where
        goPackages = either (fail . show) return
                   . mapM simpleParse
                   . Map.mapKeysWith const mkPackageName

data BuildConstraints = BuildConstraints
    { bcPackages           :: Set PackageName
    -- ^ This does not include core packages.
    , bcPackageConstraints :: PackageName -> PackageConstraints

    , bcSystemInfo         :: SystemInfo
    }

data PackageConstraints = PackageConstraints
    { pcVersionRange    :: VersionRange
    , pcMaintainer      :: Maybe Maintainer
    , pcTests           :: TestState
    , pcHaddocks        :: TestState
    , pcBuildBenchmarks :: Bool
    , pcFlagOverrides   :: Map FlagName Bool
    }
    deriving (Show, Eq)
instance ToJSON PackageConstraints where
    toJSON PackageConstraints {..} = object $ addMaintainer
        [ "version-range" .= display pcVersionRange
        , "tests" .= pcTests
        , "haddocks" .= pcHaddocks
        , "build-benchmarks" .= pcBuildBenchmarks
        , "flags" .= Map.mapKeysWith const unFlagName pcFlagOverrides
        ]
      where
        addMaintainer = maybe id (\m -> (("maintainer" .= m):)) pcMaintainer
instance FromJSON PackageConstraints where
    parseJSON = withObject "PackageConstraints" $ \o -> do
        pcVersionRange <- (o .: "version-range")
                      >>= either (fail . show) return . simpleParse
        pcTests <- o .: "tests"
        pcHaddocks <- o .: "haddocks"
        pcBuildBenchmarks <- o .: "build-benchmarks"
        pcFlagOverrides <- Map.mapKeysWith const mkFlagName <$> o .: "flags"
        pcMaintainer <- o .:? "maintainer"
        return PackageConstraints {..}

-- | The proposed plan from the requirements provided by contributors.
defaultBuildConstraints :: IO BuildConstraints
defaultBuildConstraints = do
    siCorePackages <- getCorePackages
    siCoreExecutables <- getCoreExecutables
    siGhcVersion <- getGhcVersion
    oldGhcVer <-
        case siGhcVersion of
            Version (x:y:_) _ -> return $ Old.GhcMajorVersion x y
            _ -> error $ "Didn't not understand GHC version: " ++ show siGhcVersion


    let oldSettings = Old.defaultSelectSettings oldGhcVer False
        oldStable = Old.defaultStablePackages oldGhcVer False
        defaultGlobalFlags = asMap $ mapFromList $
            map (, True) (map FlagName $ setToList $ Old.flags oldSettings mempty) ++
            map (, False) (map FlagName $ setToList $ Old.disabledFlags oldSettings)
        expectedFailures = Old.defaultExpectedFailures oldGhcVer False ++
                           newExpectedFailures
        skippedTests =
            old ++ extraSkippedTests
          where
            old = setFromList $ map unPackageName $ setToList $ Old.skippedTests oldSettings

        bcPackages = Map.keysSet oldStable
        bcPackageConstraints name =
            PackageConstraints {..}
          where
            mold = lookup name $ oldStable

            pcVersionRange = simplifyVersionRange $ maybe anyVersion fst mold
            pcMaintainer = (Maintainer . pack . Old.unMaintainer . snd) <$> mold
            pcTests
                | unPackageName name `member` skippedTests = Don'tBuild
                | name `member` expectedFailures = ExpectFailure
                | otherwise = ExpectSuccess

            pcBuildBenchmarks = unPackageName name `notMember` skippedBenchs

            -- FIXME ultimately separate haddock and test failures in specification
            pcHaddocks
                | name `member` expectedFailures = ExpectFailure
                | otherwise = ExpectSuccess

            pcFlagOverrides = packageFlags name ++ defaultGlobalFlags

        -- FIXME consider not hard-coding the next two values
        siOS   = Distribution.System.Linux
        siArch = Distribution.System.X86_64

        bcSystemInfo = SystemInfo {..}

    return BuildConstraints {..}

packageFlags :: PackageName -> Map FlagName Bool
packageFlags (PackageName "mersenne-random-pure64") = singletonMap (FlagName "small_base") False
packageFlags _ = mempty

extraSkippedTests :: HashSet Text
extraSkippedTests = setFromList $ words =<<
    [ "HTTP Octree options"
    , "hasql"
    , "bloodhound fb" -- require old hspec
    , "diagrams-haddock" -- requires old tasty
    , "hasql-postgres" -- requires old hasql
    ]

skippedBenchs :: HashSet Text
skippedBenchs = setFromList $ words =<<
    [ "machines criterion-plus graphviz lifted-base pandoc stm-containers uuid"
    , "cases hasql-postgres" -- pulls in criterion-plus, which has restrictive upper bounds
    ]

newExpectedFailures :: Set PackageName
newExpectedFailures = setFromList $ map PackageName $ words =<<
    [ "cautious-file" -- weird problems with cabal test
    ]
