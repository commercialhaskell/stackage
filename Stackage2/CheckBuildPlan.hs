{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Confirm that a build plan has a consistent set of dependencies.
module Stackage2.CheckBuildPlan
    ( checkBuildPlan
    ) where

import Stackage2.Prelude
import Stackage2.BuildPlan
import Stackage2.PackageDescription
import Control.Monad.Writer.Strict (execWriter, Writer, tell)
import Distribution.Version            (intersectVersionRanges, withinRange)

checkBuildPlan :: MonadThrow m => BuildPlan FlatComponent -> m ()
checkBuildPlan BuildPlan {..}
    | null errs' = return ()
    | otherwise = throwM errs
  where
    allPackages = bpCore ++ map pbVersion bpExtra
    errs@(BadBuildPlan errs') =
        execWriter $ mapM_ (checkDeps allPackages) $ mapToList bpExtra

checkDeps :: Map PackageName Version
          -> (PackageName, PackageBuild FlatComponent)
          -> Writer BadBuildPlan ()
checkDeps allPackages (user, pb) =
    mapM_ go $ mapToList $ fcDeps $ pbDesc pb
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
        errMap = singletonMap user range

newtype BadBuildPlan = BadBuildPlan (Map (PackageName, Maybe Version) (Map PackageName VersionRange)) -- FIXME add maintainer and Github ping info
    deriving Typeable
instance Exception BadBuildPlan
instance Show BadBuildPlan where
    show (BadBuildPlan errs) =
        concatMap go $ mapToList errs
      where
        go ((dep, mdepVer), users) = unlines
            $ showDepVer dep mdepVer
            : map showUser (mapToList users)

        showDepVer :: PackageName -> Maybe Version -> String
        showDepVer dep Nothing = display dep ++ " (not present) depended on by:"
        showDepVer dep (Just version) = concat
            [ display dep
            , "-"
            , display version
            , " depended on by:"
            ]

        showUser :: (PackageName, VersionRange) -> String
        showUser (user, range) = concat
            [ "- "
            , display user
            , " ("
            , display range
            , ")"
            ]

instance Monoid BadBuildPlan where
    mempty = BadBuildPlan mempty
    mappend (BadBuildPlan x) (BadBuildPlan y) =
        BadBuildPlan $ unionWith (unionWith intersectVersionRanges) x y
