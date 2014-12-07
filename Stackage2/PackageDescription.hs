{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
    , CheckCond (..)
    ) where

import Distribution.Package            (Dependency (..))
import Distribution.PackageDescription
import Stackage2.CorePackages
import Stackage2.PackageConstraints
import Stackage2.PackageIndex
import Stackage2.Prelude
import Stackage2.GithubPings
import Control.Monad.State.Strict (execState, get, put)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Aeson
import Distribution.System (OS, Arch)
import Distribution.Compiler (CompilerFlavor)

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
    :: MonadThrow m
    => CheckCond
    -> Bool -- ^ include test suites?
    -> Bool -- ^ include benchmarks?
    -> GenericPackageDescription
    -> m FlatComponent
getFlattenedComponent checkCond' includeTests includeBench gpd =
    liftM fold
        $ mapM (flattenComponent checkCond')
        $ getSimpleTrees includeTests includeBench gpd

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

flattenComponent :: MonadThrow m => CheckCond -> SimpleTree -> m FlatComponent
flattenComponent checkCond' (SimpleTree deps conds extra) = do
    conds' <- mapM goCond conds
    return $ mconcat $ here : conds'
  where
    here = FlatComponent { fcDeps = deps, fcExtra = extra }
    goCond (cond, tree1, mtree2) = do
        b <- checkCond checkCond' cond
        if b
            then flattenComponent checkCond' tree1
            else maybe (return mempty) (flattenComponent checkCond') mtree2

checkCond :: MonadThrow m => CheckCond -> Condition ConfVar -> m Bool
checkCond CheckCond {..} cond0 =
    go cond0
  where
    go (Var (OS os)) = return $ os == ccOS
    go (Var (Arch arch)) = return $ arch == ccArch
    go (Var (Flag flag)) =
        case lookup flag ccFlags of
            Nothing -> throwM $ FlagNotDefined ccPackageName flag cond0
            Just b -> return b
    go (Var (Impl flavor range)) = return
                                 $ flavor == ccCompilerFlavor
                                && ccCompilerVersion `withinRange` range
    go (Lit b) = return b
    go (CNot c) = not `liftM` go c
    go (CAnd x y) = (&&) `liftM` go x `ap` go y
    go (COr x y) = (||) `liftM` go x `ap` go y

data CheckCondException = FlagNotDefined PackageName FlagName (Condition ConfVar)
    deriving (Show, Typeable)
instance Exception CheckCondException

data CheckCond = CheckCond
    { ccPackageName :: PackageName -- for debugging only
    , ccOS :: OS
    , ccArch :: Arch
    , ccFlags :: Map FlagName Bool
    , ccCompilerFlavor :: CompilerFlavor
    , ccCompilerVersion :: Version
    }
