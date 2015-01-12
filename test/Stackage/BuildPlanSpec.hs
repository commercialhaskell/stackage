{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Stackage.BuildPlanSpec (spec) where

import qualified Data.Map as Map
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Yaml
import qualified Data.Yaml as Y
import           Distribution.Version
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Stackage.BuildConstraints
import           Stackage.BuildPlan
import           Stackage.CheckBuildPlan
import           Stackage.CompleteBuild
import           Stackage.PackageDescription
import           Stackage.PerformBuild
import           Stackage.Prelude
import qualified Stackage.ShakeBuild as Shake
import           Stackage.UpdateBuildPlan
import           Test.Hspec

spec :: Spec
spec = do
    it "simple package set" $ check testBuildConstraints $ makePackageSet
        [("foo", [0, 0, 0], [("bar", thisV [0, 0, 0])])
        ,("bar", [0, 0, 0], [])]
    it "bad version range on depdendency fails" $ badBuildPlan $ makePackageSet
        [("foo", [0, 0, 0], [("bar", thisV [1, 1, 0])])
        ,("bar", [0, 0, 0], [])]
    it "nonexistent package fails to check" $ badBuildPlan $ makePackageSet
        [("foo", [0, 0, 0], [("nonexistent", thisV [0, 0, 0])])
        ,("bar", [0, 0, 0], [])]
    it "mutual cycles fail to check" $ badBuildPlan $ makePackageSet
        [("foo", [0, 0, 0], [("bar", thisV [0, 0, 0])])
        ,("bar", [0, 0, 0], [("foo", thisV [0, 0, 0])])]
    it "nested cycles fail to check" $ badBuildPlan $ makePackageSet
        [("foo", [0, 0, 0], [("bar", thisV [0, 0, 0])])
        ,("bar", [0, 0, 0], [("mu", thisV [0, 0, 0])])
        ,("mu", [0, 0, 0], [("foo", thisV [0, 0, 0])])]
    {- Shouldn't be testing this actually
    it "basic build" $ basicBuild $ makePackageSet
        [("acme-strtok", [0,1,0,3], [("mtl", thisV [2, 2, 1])])
        ,("acme-dont", [1,1], [])
        ,("mtl",[2,2,1],[("base",anyV)
                        ,("transformers",anyV)])
        ,("transformers",[0,4,1,0],[("base",anyV)])]
    it "shake build" $ shakeBuild $ makePackageSet
        [("acme-strtok", [0,1,0,3], [("mtl", thisV [2, 1, 3, 1])])
        ,("acme-dont", [1,1], [])
        ,("mtl",[2,1,3,1],[("base",anyV)
                          ,("transformers",anyV)])
        ,("transformers",[0,3,0,0],[("base",anyV)])]
    it "default package set checks ok" $
      check defaultBuildConstraints getLatestAllowedPlans
    -}

-- | Checking should be considered a bad build plan.
badBuildPlan :: (BuildConstraints -> IO (Map PackageName PackagePlan))
             -> void
             -> IO ()
badBuildPlan m _ = do
    mu <- try (check testBuildConstraints m)
    case mu of
        Left (_ :: BadBuildPlan) ->
            return ()
        Right () ->
            error "Expected bad build plan."

-- | Perform a basic build.
basicBuild :: (BuildConstraints -> IO (Map PackageName PackagePlan))
             -> void
             -> IO ()
basicBuild getPlans _ = do
    withManager
        tlsManagerSettings
        (\man ->
              do settings@Settings{..} <- getTestSettings man
                                                          Nightly
                                                          fullBuildConstraints
                                                          getPlans
                 let pb = (getPerformBuild buildFlags settings)
                 logs <- performBuild
                             pb
                 mapM_ putStrLn logs)
    where buildType =
              Nightly
          buildFlags =
              BuildFlags
                { bfEnableTests      = False
                , bfDoUpload         = False
                , bfEnableLibProfile = False
                , bfVerbose          = False
                }

-- | Perform a shake build.
shakeBuild :: (BuildConstraints -> IO (Map PackageName PackagePlan))
             -> void
             -> IO ()
shakeBuild getPlans _ = do
    withManager
        tlsManagerSettings
        (\man ->
              do settings@Settings{..} <- getTestSettings
                                              man
                                              Nightly
                                              fullBuildConstraints
                                              getPlans
                 let pb =
                         (getPerformBuild buildFlags settings)
                 Shake.performBuild pb)
    where buildType =
              Nightly
          buildFlags =
              BuildFlags {bfEnableTests = False
                         ,bfDoUpload = False
                         ,bfEnableLibProfile = False
                         ,bfVerbose = False}

-- | Check build plan with the given package set getter.
check :: (Manager -> IO BuildConstraints)
      -> (BuildConstraints -> IO (Map PackageName PackagePlan))
      -> IO ()
check readPlanFile getPlans = withManager tlsManagerSettings $ \man -> do
    bc <- readPlanFile man
    plans <- getPlans bc
    bp <- newBuildPlan plans bc
    let bs = Y.encode bp
        ebp' = Y.decodeEither bs

    bp' <- either error return ebp'

    let allPackages = Map.keysSet (bpPackages bp) ++ Map.keysSet (bpPackages bp')
    forM_ allPackages $ \name ->
        (name, lookup name (bpPackages bp')) `shouldBe`
        (name, lookup name (bpPackages bp))
    bpGithubUsers bp' `shouldBe` bpGithubUsers bp

    when (bp' /= bp) $ error "bp' /= bp"
    bp2 <- updateBuildPlan plans bp
    when (dropVersionRanges bp2 /= dropVersionRanges bp) $ error "bp2 /= bp"
    checkBuildPlan bp
  where
    dropVersionRanges bp =
        bp { bpPackages = map go $ bpPackages bp }
      where
        go pb = pb { ppConstraints = go' $ ppConstraints pb }
        go' pc = pc { pcVersionRange = anyVersion }

-- | Make a package set from a convenient data structure.
makePackageSet
    :: [(String,[Int],[(String,VersionRange)])]
    -> BuildConstraints
    -> IO (Map PackageName PackagePlan)
makePackageSet ps _ =
    return $
    M.fromList $
    map
        (\(name,ver,deps) ->
              ( PackageName name
              , dummyPackage ver $
                M.fromList $
                map
                    (\(dname,dver) ->
                          ( PackageName dname
                          , DepInfo {diComponents = S.fromList
                                             [CompLibrary]
                                    ,diRange = dver}))
                    deps))
        ps
    where
        dummyPackage v deps =
            PackagePlan
                {ppVersion = Version v []
                ,ppGithubPings = mempty
                ,ppUsers = mempty
                ,ppConstraints =
                    PackageConstraints
                        {pcVersionRange = anyV
                        ,pcMaintainer = Nothing
                        ,pcTests = Don'tBuild
                        ,pcHaddocks = Don'tBuild
                        ,pcBuildBenchmarks = False
                        ,pcFlagOverrides = mempty
                        ,pcEnableLibProfile = False}
                ,ppDesc =
                    SimpleDesc
                        {sdPackages = deps
                        ,sdTools = mempty
                        ,sdProvidedExes = mempty
                        ,sdModules = mempty}}

-- | This exact version is required.
thisV :: [Int] -> VersionRange
thisV ver = thisVersion (Version ver [])

-- | Accept any version.
anyV :: VersionRange
anyV = anyVersion

-- | Get settings for doing test builds.
getTestSettings :: Manager -> BuildType -> (Manager -> IO BuildConstraints) -> (BuildConstraints -> IO (Map PackageName PackagePlan)) -> IO Settings
getTestSettings man Nightly readPlanFile getPlans = do
    day <- tshow . utctDay <$> getCurrentTime
    bc <- readPlanFile man
    plans <- getPlans bc
    bp <- newBuildPlan plans bc
    return $ nightlySettings day bp

-- | Test plan.
fullBuildConstraints :: void -> IO BuildConstraints
fullBuildConstraints _ =
    decodeFileEither
        (fpToString fp) >>=
    either throwIO toBC
    where fp = "test/full-build-constraints.yaml"

-- | Test plan.
testBuildConstraints :: void -> IO BuildConstraints
testBuildConstraints _ =
    decodeFileEither
        (fpToString fp) >>=
    either throwIO toBC
    where fp = "test/test-build-constraints.yaml"
