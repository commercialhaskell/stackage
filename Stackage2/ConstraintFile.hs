{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, RecordWildCards #-}
module Stackage2.ConstraintFile
    ( loadBuildConstraints
    ) where

import Stackage2.Prelude
import Data.Yaml (decodeFileEither)
import Stackage2.BuildConstraints
import Data.Aeson
import qualified Data.Map as Map
import Distribution.Package (Dependency (..))
import Distribution.Version (anyVersion)
import Control.Monad.Writer.Strict (execWriter, tell)

loadBuildConstraints fp = decodeFileEither fp >>= either throwIO toBC

data ConstraintFile = ConstraintFile
    { cfGlobalFlags :: Map FlagName Bool
    , cfPackageFlags :: Map PackageName (Map FlagName Bool)
    , cfSkippedTests :: Set PackageName
    , cfExpectedTestFailures :: Set PackageName
    , cfExpectedHaddockFailures :: Set PackageName
    , cfSkippedBenchmarks :: Set PackageName
    , cfPackages :: Map Maintainer (Vector Dependency)
    }

instance FromJSON ConstraintFile where
    parseJSON = withObject "ConstraintFile" $ \o -> do
        cfGlobalFlags <- goFlagMap <$> o .: "global-flags"
        cfPackageFlags <- (goPackageMap . fmap goFlagMap) <$> o .: "package-flags"
        cfSkippedTests <- getPackages o "skipped-tests"
        cfExpectedTestFailures <- getPackages o "expected-test-failures"
        cfExpectedHaddockFailures <- getPackages o "expected-haddock-failures"
        cfSkippedBenchmarks <- getPackages o "skipped-benchmarks"
        cfPackages <- o .: "packages"
                  >>= mapM (mapM toDep)
                    . Map.mapKeysWith const Maintainer
        return ConstraintFile {..}
      where
        goFlagMap = Map.mapKeysWith const FlagName
        goPackageMap = Map.mapKeysWith const PackageName
        getPackages o name = (setFromList . map PackageName) <$> o .: name

        toDep :: Monad m => Text -> m Dependency
        toDep = either (fail . show) return . simpleParse

toBC :: ConstraintFile -> IO BuildConstraints
toBC ConstraintFile {..} = do
    bcSystemInfo <- getSystemInfo
    return BuildConstraints {..}
  where
    combine (maintainer, range1) (_, range2) =
        (maintainer, intersectVersionRanges range1 range2)
    revmap = unionsWith combine $ ($ []) $ execWriter
           $ forM_ (mapToList cfPackages)
           $ \(maintainer, deps) -> forM_ deps
           $ \(Dependency name range) ->
            tell (singletonMap name (maintainer, range):)

    bcPackages = Map.keysSet revmap

    bcPackageConstraints name =
        PackageConstraints {..}
      where
        mpair = lookup name revmap
        pcMaintainer = fmap fst mpair
        pcVersionRange = maybe anyVersion snd mpair
        pcTests
            | name `member` cfSkippedTests = Don'tBuild
            | name `member` cfExpectedTestFailures = ExpectFailure
            | otherwise = ExpectSuccess
        pcBuildBenchmarks = name `notMember` cfSkippedBenchmarks
        pcHaddocks
            | name `member` cfExpectedHaddockFailures = ExpectFailure

            -- Temporary to match old behavior
            | name `member` cfExpectedTestFailures = ExpectFailure

            | otherwise = ExpectSuccess
        pcFlagOverrides = fromMaybe mempty (lookup name cfPackageFlags) ++
                          cfGlobalFlags
