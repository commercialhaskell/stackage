{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
-- | Manipulate @GenericPackageDescription@ from Cabal into something more
-- useful for us.
module Stackage2.PackageDescription
    ( FlatComponent (..)
    , getFlattenedComponent
    , SimpleExtra (..)
    ) where

import Distribution.Package            (Dependency (..))
import Distribution.PackageDescription
import Distribution.Version            (withinRange, intersectVersionRanges)
import Stackage2.CorePackages
import Stackage2.PackageConstraints
import Stackage2.PackageIndex
import Stackage2.Prelude
import Stackage2.GithubPings
import Control.Monad.State.Strict (execState, get, put)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Aeson

data SimpleTree = SimpleTree
    { stDeps :: Map PackageName VersionRange
    , stConds :: [(Condition ConfVar, SimpleTree, Maybe SimpleTree)]
    , stExtra :: SimpleExtra
    }
    deriving Show
instance Monoid SimpleTree where
    mempty = SimpleTree mempty mempty mempty
    mappend (SimpleTree a b c) (SimpleTree x y z) = SimpleTree
        (unionWith intersectVersionRanges a x)
        (b ++ y)
        (c ++ z)

data SimpleExtra = SimpleExtra
    { seTools :: Map PackageName VersionRange
    }
    deriving Show
instance Monoid SimpleExtra where
    mempty = SimpleExtra mempty
    mappend (SimpleExtra a) (SimpleExtra x) = SimpleExtra
        (unionWith intersectVersionRanges a x)

getFlattenedComponent
    :: Bool -- ^ include test suites?
    -> Bool -- ^ include benchmarks?
    -> GenericPackageDescription
    -> FlatComponent
getFlattenedComponent includeTests includeBench gpd =
    foldMap flattenComponent $ getSimpleTrees includeTests includeBench gpd

getSimpleTrees :: Bool -- ^ include test suites?
               -> Bool -- ^ include benchmarks?
               -> GenericPackageDescription
               -> [SimpleTree]
getSimpleTrees includeTests includeBench gpd = concat
    [ maybe [] (return . go libBuildInfo) $ condLibrary gpd
    , map (go buildInfo . snd) $ condExecutables gpd
    , if includeTests
        then map (go testBuildInfo . snd) $ condTestSuites gpd
        else []
    , if includeBench
        then map (go benchmarkBuildInfo . snd) $ condBenchmarks gpd
        else []
    ]
  where
    go getExtra (CondNode dat deps comps) = SimpleTree
        { stDeps = unionsWith intersectVersionRanges
                 $ map (\(Dependency x y) -> singletonMap x y) deps
        , stConds = map (goComp getExtra) comps
        , stExtra = toSimpleExtra $ getExtra dat
        }

    goComp getExtra (cond, tree1, mtree2) =
        (cond, go getExtra tree1, go getExtra <$> mtree2)

    toSimpleExtra bi = SimpleExtra
        { seTools = unionsWith intersectVersionRanges $ flip map (buildTools bi)
            $ \(Dependency name range) -> singletonMap name range
        }

data FlatComponent = FlatComponent
    { fcDeps :: Map PackageName VersionRange
    , fcExtra :: SimpleExtra
    }
    deriving Show
instance Monoid FlatComponent where
    mempty = FlatComponent mempty mempty
    mappend (FlatComponent a b) (FlatComponent x y) = FlatComponent
        (unionWith intersectVersionRanges a x)
        (b ++ y)

flattenComponent :: SimpleTree -> FlatComponent
flattenComponent (SimpleTree deps conds extra) =
    mconcat $ here : map goCond conds
  where
    here = FlatComponent { fcDeps = deps, fcExtra = extra }
    goCond (cond, tree1, mtree2)
        | checkCond cond = flattenComponent tree1
        | otherwise = maybe mempty flattenComponent mtree2

checkCond :: Condition ConfVar -> Bool
checkCond _ = False -- FIXME
