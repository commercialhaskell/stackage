{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
-- | Confirm that a build plan has a consistent set of dependencies.
module Stackage2.CheckBuildPlan
    ( checkBuildPlan
    ) where

import Control.Monad.Writer.Strict  (Writer, execWriter, tell)
import Stackage2.BuildConstraints
import Stackage2.BuildPlan
import Stackage2.PackageDescription
import Stackage2.Prelude

checkBuildPlan :: MonadThrow m => BuildPlan -> m ()
checkBuildPlan BuildPlan {..}
    | null errs' = return ()
    | otherwise = throwM errs
  where
    allPackages = siCorePackages bpSystemInfo ++ map ppVersion bpPackages
    errs@(BadBuildPlan errs') =
        execWriter $ mapM_ (checkDeps allPackages) $ mapToList bpPackages

checkDeps :: Map PackageName Version
          -> (PackageName, PackagePlan)
          -> Writer BadBuildPlan ()
checkDeps allPackages (user, pb) =
    mapM_ go $ mapToList $ sdPackages $ ppDesc pb
  where
    go (dep, range) =
        case lookup dep allPackages of
            Nothing -> tell $ BadBuildPlan $ singletonMap (dep, Nothing) errMap
            Just version
                | version `withinRange` range -> return ()
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
            $ showDepVer dep mdepVer
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
            , display range
            , "). "
            , pkgUserShow2 pu
            ]

instance Monoid BadBuildPlan where
    mempty = BadBuildPlan mempty
    mappend (BadBuildPlan x) (BadBuildPlan y) =
        BadBuildPlan $ unionWith (unionWith intersectVersionRanges) x y
