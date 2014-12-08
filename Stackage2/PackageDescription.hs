{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}
-- | Manipulate @GenericPackageDescription@ from Cabal into something more
-- useful for us.
module Stackage2.PackageDescription
    ( SimpleDesc (..)
    , toSimpleDesc
    , CheckCond (..)
    ) where

import           Control.Monad.Writer.Strict     (MonadWriter, execWriterT,
                                                  tell)
import           Data.Aeson
import qualified Data.Map                        as Map
import           Distribution.Compiler           (CompilerFlavor)
import           Distribution.Package            (Dependency (..))
import           Distribution.PackageDescription
import           Distribution.System             (Arch, OS)
import           Stackage2.Prelude

-- | A simplified package description that tracks:
--
-- * Package dependencies
--
-- * Build tool dependencies
--
-- * Provided executables
--
-- It has fully resolved all conditionals
data SimpleDesc = SimpleDesc
    { sdPackages     :: Map PackageName VersionRange
    , sdTools        :: Map ExeName VersionRange
    , sdProvidedExes :: Set ExeName
    }
    deriving (Show, Eq)
instance Monoid SimpleDesc where
    mempty = SimpleDesc mempty mempty mempty
    mappend (SimpleDesc a b c) (SimpleDesc x y z) = SimpleDesc
        (unionWith intersectVersionRanges a x)
        (unionWith intersectVersionRanges b y)
        (c ++ z)
instance ToJSON SimpleDesc where
    toJSON SimpleDesc {..} = object
        [ "packages" .= Map.mapKeysWith const unPackageName (map display sdPackages)
        , "tools" .= Map.mapKeysWith const unExeName (map display sdTools)
        , "provided-exes" .= sdProvidedExes
        ]
instance FromJSON SimpleDesc where
    parseJSON = withObject "SimpleDesc" $ \o -> do
        sdPackages <- (o .: "packages") >>=
                      either (fail . show) return .
                      mapM simpleParse .
                      Map.mapKeysWith const mkPackageName
        sdTools <- (o .: "tools") >>=
                   either (fail . show) return .
                   mapM simpleParse .
                   Map.mapKeysWith const ExeName
        sdProvidedExes <- o .: "provided-exes"
        return SimpleDesc {..}

-- | Convert a 'GenericPackageDescription' into a 'SimpleDesc' by following the
-- constraints in the provided 'CheckCond'.
toSimpleDesc :: MonadThrow m
             => CheckCond
             -> GenericPackageDescription
             -> m SimpleDesc
toSimpleDesc cc gpd = execWriterT $ do
    forM_ (condLibrary gpd) $ tellTree cc libBuildInfo
    forM_ (condExecutables gpd) $ tellTree cc buildInfo . snd
    tell mempty { sdProvidedExes = setFromList
                                 $ map (fromString . fst)
                                 $ condExecutables gpd
                }
    when (ccIncludeTests cc) $ forM_ (condTestSuites gpd)
        $ tellTree cc testBuildInfo . snd
    when (ccIncludeBenchmarks cc) $ forM_ (condBenchmarks gpd)
        $ tellTree cc benchmarkBuildInfo . snd

-- | Convert a single CondTree to a 'SimpleDesc'.
tellTree :: (MonadWriter SimpleDesc m, MonadThrow m)
         => CheckCond
         -> (a -> BuildInfo)
         -> CondTree ConfVar [Dependency] a
         -> m ()
tellTree cc getBI (CondNode dat deps comps) = do
    tell mempty
        { sdPackages = unionsWith intersectVersionRanges $ flip map deps
            $ \(Dependency x y) -> singletonMap x $ simplifyVersionRange y
        , sdTools = unionsWith intersectVersionRanges $ flip map (buildTools $ getBI dat)
            $ \(Dependency name range) -> singletonMap
                -- In practice, cabal files refer to the exe name, not the
                -- package name.
                (ExeName $ unPackageName name)
                (simplifyVersionRange range)
        }
    forM_ comps $ \(cond, ontrue, onfalse) -> do
        b <- checkCond cc cond
        if b
            then tellTree cc getBI ontrue
            else maybe (return ()) (tellTree cc getBI) onfalse

-- | Resolve a condition to a boolean based on the provided 'CheckCond'.
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
    { ccPackageName       :: PackageName -- for debugging only
    , ccOS                :: OS
    , ccArch              :: Arch
    , ccFlags             :: Map FlagName Bool
    , ccCompilerFlavor    :: CompilerFlavor
    , ccCompilerVersion   :: Version
    , ccIncludeTests      :: Bool
    , ccIncludeBenchmarks :: Bool
    }
