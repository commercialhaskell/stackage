{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The constraints on package selection for a new build plan.
module Stackage2.PackageConstraints
    ( PackageConstraints (..)
    , TestState (..)
    , defaultPackageConstraints
    ) where

import           Stackage2.Prelude
import           Stackage2.CorePackages
import qualified Stackage.Config as Old
import qualified Stackage.Types  as Old
import qualified Stackage.Select as Old
import Data.Aeson (ToJSON (..), FromJSON (..), withText)
import Distribution.System (OS, Arch)
import qualified Distribution.System

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

data PackageConstraints = PackageConstraints
    { pcPackages :: Map PackageName (VersionRange, Maybe Maintainer)
    -- ^ This does not include core packages or dependencies, just packages
    -- added by some maintainer.
    , pcGhcVersion :: Version
    , pcOS :: OS
    , pcArch :: Arch
    , pcCorePackages :: Map PackageName Version
    , pcCoreExecutables :: Set ExeName

    -- Have a single lookup function with all of the package-specific stuff?
    , pcTests :: PackageName -> TestState
    , pcHaddocks :: PackageName -> TestState
    , pcBuildBenchmark :: PackageName -> Bool
    , pcFlagOverrides :: PackageName -> Map FlagName Bool
    }

-- | The proposed plan from the requirements provided by contributors.
defaultPackageConstraints :: IO PackageConstraints
defaultPackageConstraints = do
    core <- getCorePackages
    coreExes <- getCoreExecutables
    ghcVer <- getGhcVersion
    oldGhcVer <-
        case ghcVer of
            Version (x:y:_) _ -> return $ Old.GhcMajorVersion x y
            _ -> error $ "Didn't not understand GHC version: " ++ show ghcVer


    let oldSettings = Old.defaultSelectSettings oldGhcVer False
        defaultGlobalFlags = asMap $ mapFromList $
            map (, True) (map FlagName $ setToList $ Old.flags oldSettings mempty) ++
            map (, False) (map FlagName $ setToList $ Old.disabledFlags oldSettings)
        tryBuildTest (PackageName name) = pack name `notMember` skippedTests
        tryBuildBenchmark (PackageName name) = pack name `notMember` skippedBenchs
        expectedFailures = Old.defaultExpectedFailures oldGhcVer False
        skippedTests =
            old ++ extraSkippedTests
          where
            old = setFromList $ map unPackageName $ setToList $ Old.skippedTests oldSettings

    return PackageConstraints
        { pcPackages = fmap (Just . Maintainer . pack . Old.unMaintainer)
                   <$> Old.defaultStablePackages oldGhcVer False
        , pcCorePackages = core
        , pcCoreExecutables = coreExes
        , pcOS = Distribution.System.Linux -- FIXME don't hard-code?
        , pcArch = Distribution.System.X86_64
        , pcGhcVersion = ghcVer
        , pcTests = \name ->
            case () of
                ()
                    | not $ tryBuildTest name -> Don'tBuild
                    | name `member` expectedFailures -> ExpectFailure
                    | otherwise -> ExpectSuccess
        , pcBuildBenchmark = (`notMember` skippedBenchs) . unPackageName
        , pcFlagOverrides = \name -> packageFlags name ++ defaultGlobalFlags
        , pcHaddocks = \name ->
            case () of
                ()
                    | name `member` expectedFailures
                        -> ExpectFailure
                    | otherwise -> ExpectSuccess
        }

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
