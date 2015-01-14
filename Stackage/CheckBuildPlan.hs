{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ViewPatterns               #-}
-- | Confirm that a build plan has a consistent set of dependencies.
module Stackage.CheckBuildPlan
    ( checkBuildPlan
    , libAndExe
    , BadBuildPlan
    ) where

import           Control.Monad.Writer.Strict (Writer, execWriter, tell)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Stackage.BuildConstraints
import           Stackage.BuildPlan
import           Stackage.PackageDescription
import           Stackage.Prelude

-- | Check the build plan for missing deps, wrong versions, etc.
checkBuildPlan :: (MonadThrow m) => BuildPlan -> m ()
checkBuildPlan BuildPlan {..}
    | null errs' = return ()
    | otherwise = throwM errs
  where
    allPackages = map (,mempty) (siCorePackages bpSystemInfo) ++
                  map (ppVersion &&& M.keys . M.filter libAndExe . sdPackages . ppDesc) bpPackages
    errs@(BadBuildPlan errs') =
        execWriter $ mapM_ (checkDeps allPackages) $ mapToList bpPackages


-- Only looking at libraries and executables, benchmarks and tests
-- are allowed to create cycles (e.g. test-framework depends on
-- text, which uses test-framework in its test-suite).
libAndExe :: DepInfo -> Bool
libAndExe (DepInfo cs _) = any (flip elem [CompLibrary,CompExecutable]) cs

-- | For a given package name and plan, check that its dependencies are:
--
-- 1. Existent (existing in the provided package map)
-- 2. Within version range
-- 3. Check for dependency cycles.
checkDeps :: Map PackageName (Version,[PackageName])
          -> (PackageName, PackagePlan)
          -> Writer BadBuildPlan ()
checkDeps allPackages (user, pb) =
    mapM_ go $ mapToList $ sdPackages $ ppDesc pb
  where
    go (dep, diRange -> range) =
        case lookup dep allPackages of
            Nothing -> tell $ BadBuildPlan $ singletonMap (dep, Nothing) errMap
            Just (version,deps)
                | version `withinRange` range ->
                    occursCheck allPackages
                                (\d v ->
                                     tell $ BadBuildPlan $ singletonMap
                                     (d,v)
                                     errMap)
                                dep
                                deps
                                []
                | otherwise -> tell $ BadBuildPlan $ singletonMap
                    (dep, Just version)
                    errMap
      where
        errMap = singletonMap pu range
        pu = PkgUser
            { puName = user
            , puVersion = ppVersion pb
            , puMaintainer = pcMaintainer $ ppConstraints pb
            , puGithubPings = ppGithubPings pb
            }

-- | Check whether the package(s) occurs within its own dependency
-- tree.
occursCheck
    :: Monad m
    => Map PackageName (Version,[PackageName])
    -- ^ All packages.
    -> (PackageName -> Maybe Version -> m ())
    -- ^ Report an erroneous package.
    -> PackageName
    -- ^ Starting package to check for cycles in.
    -> [PackageName]
    -- ^ Dependencies of the package.
    -> [PackageName]
    -- ^ Previously seen packages up the dependency tree.
    -> m ()
occursCheck allPackages reportError =
    go
    where
        go pkg deps seen =
            case find (flip elem seen) deps of
                Just cyclic ->
                    reportError cyclic $
                    fmap fst (lookup cyclic allPackages)
                Nothing ->
                    forM_ deps $
                    \pkg' ->
                         case lookup pkg' allPackages of
                             Just (_v,deps')
                                 | pkg' /= pkg -> go pkg' deps' seen'
                             _ -> return ()
            where seen' = pkg : seen

data PkgUser = PkgUser
    { puName        :: PackageName
    , puVersion     :: Version
    , puMaintainer  :: Maybe Maintainer
    , puGithubPings :: Set Text
    }
    deriving (Eq, Ord)

pkgUserShow1 :: PkgUser -> Text
pkgUserShow1 PkgUser {..} = concat
    [ display puName
    , "-"
    , display puVersion
    ]

pkgUserShow2 :: PkgUser -> Text
pkgUserShow2 PkgUser {..} = unwords
    $ (maybe "No maintainer" unMaintainer puMaintainer ++ ".")
    : map (cons '@') (setToList puGithubPings)

newtype BadBuildPlan =
    BadBuildPlan (Map (PackageName, Maybe Version) (Map PkgUser VersionRange))
    deriving Typeable
instance Exception BadBuildPlan
instance Show BadBuildPlan where
    show (BadBuildPlan errs) =
        unpack $ concatMap go $ mapToList errs
      where
        go ((dep, mdepVer), users) = unlines
            $ ""
            : showDepVer dep mdepVer
            : map showUser (mapToList users)

        showDepVer :: PackageName -> Maybe Version -> Text
        showDepVer dep Nothing = display dep ++ " (not present) depended on by:"
        showDepVer dep (Just version) = concat
            [ display dep
            , "-"
            , display version
            , " depended on by:"
            ]

        showUser :: (PkgUser, VersionRange) -> Text
        showUser (pu, range) = concat
            [ "- "
            , pkgUserShow1 pu
            , " ("
            -- add a space after < to avoid confusing Markdown processors (like
            -- Github's issue tracker)
            , T.replace "<" "< " $ display range
            , "). "
            , pkgUserShow2 pu
            ]

instance Monoid BadBuildPlan where
    mempty = BadBuildPlan mempty
    mappend (BadBuildPlan x) (BadBuildPlan y) =
        BadBuildPlan $ unionWith (unionWith intersectVersionRanges) x y
