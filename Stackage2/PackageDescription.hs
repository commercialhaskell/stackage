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
    { seTools :: Map ExeName VersionRange
    , seProvidedExes :: Set ExeName
    }
    deriving Show
instance Monoid SimpleExtra where
    mempty = SimpleExtra mempty mempty
    mappend (SimpleExtra a b) (SimpleExtra x y) = SimpleExtra
        (unionWith intersectVersionRanges a x)
        (b ++ y)

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
    [ maybe [] (return . go libBuildInfo mempty) $ condLibrary gpd
    , map (\(x, y) -> go buildInfo (singletonSet $ ExeName $ pack x) y)
         $ condExecutables gpd
    , if includeTests
        then map (go testBuildInfo mempty . snd) $ condTestSuites gpd
        else []
    , if includeBench
        then map (go benchmarkBuildInfo mempty . snd) $ condBenchmarks gpd
        else []
    ]
  where
    go getBI exes (CondNode dat deps comps) = SimpleTree
        { stDeps = unionsWith intersectVersionRanges
                 $ map (\(Dependency x y) -> singletonMap x y) deps
        , stConds = map (goComp getBI exes) comps
        , stExtra = toSimpleExtra (getBI dat) exes
        }

    goComp getBI exes (cond, tree1, mtree2) =
        (cond, go getBI exes tree1, go getBI exes <$> mtree2)

    toSimpleExtra bi exes = SimpleExtra
        { seTools = unionsWith intersectVersionRanges $ flip map (buildTools bi)
            $ \(Dependency name range) -> singletonMap
                (ExeName $ unPackageName name)
                range
        , seProvidedExes = exes
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
