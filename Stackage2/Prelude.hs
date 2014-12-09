{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
module Stackage2.Prelude
    ( module X
    , module Stackage2.Prelude
    ) where

import           ClassyPrelude.Conduit           as X
import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.Conduit.Process            as X
import qualified Data.Map                        as Map
import           Data.Typeable                   (TypeRep, typeOf)
import           Distribution.Package            as X (PackageIdentifier (..), PackageName (PackageName))
import           Distribution.PackageDescription as X (FlagName (..), GenericPackageDescription)
import qualified Distribution.Text               as DT
import           Distribution.Version            as X (Version (..),
                                                       VersionRange)
import           Distribution.Version            as X (withinRange)
import qualified Distribution.Version            as C
import           System.Exit                     (ExitCode (ExitSuccess))

unPackageName :: PackageName -> Text
unPackageName (PackageName str) = pack str

unFlagName :: FlagName -> Text
unFlagName (FlagName str) = pack str

mkPackageName :: Text -> PackageName
mkPackageName = PackageName . unpack

mkFlagName :: Text -> FlagName
mkFlagName = FlagName . unpack

display :: DT.Text a => a -> Text
display = fromString . DT.display

simpleParse :: (MonadThrow m, DT.Text a, Typeable a) => Text -> m a
simpleParse orig = withTypeRep $ \rep ->
    case DT.simpleParse str of
        Nothing -> throwM (ParseFailedException rep (pack str))
        Just v  -> return v
  where
    str = unpack orig

    withTypeRep :: Typeable a => (TypeRep -> m a) -> m a
    withTypeRep f =
        res
      where
        res = f (typeOf (unwrap res))

        unwrap :: m a -> a
        unwrap _ = error "unwrap"

data ParseFailedException = ParseFailedException TypeRep Text
    deriving (Show, Typeable)
instance Exception ParseFailedException

#ifndef MIN_VERSION_streaming_commons
#define MIN_VERSION_streaming_commons(x, y, z) 1
#endif
#if !MIN_VERSION_streaming_commons(0,1,7)
data ProcessExitedUnsuccessfully = ProcessExitedUnsuccessfully CreateProcess ExitCode
    deriving Typeable
instance Show ProcessExitedUnsuccessfully where
    show (ProcessExitedUnsuccessfully cp ec) = concat
        [ "Process exited with "
        , show ec
        , ": "
        , showCmdSpec (cmdspec cp)
        ]
      where
        showCmdSpec (ShellCommand str) = str
        showCmdSpec (RawCommand x xs) = unwords (x:xs)
instance Exception ProcessExitedUnsuccessfully

checkExitCode :: MonadThrow m => CreateProcess -> ExitCode -> m ()
checkExitCode _ ExitSuccess = return ()
checkExitCode cp ec = throwM $ ProcessExitedUnsuccessfully cp ec

-- FIXME move into streaming-commons?
withCheckedProcess :: ( InputSource stdin
                      , OutputSink stderr
                      , OutputSink stdout
                      , MonadIO m
                      )
                   => CreateProcess
                   -> (stdin -> stdout -> stderr -> m b)
                   -> m b
withCheckedProcess cp f = do
    (x, y, z, sph) <- streamingProcess cp
    res <- f x y z
    ec <- waitForStreamingProcess sph
    liftIO $ checkExitCode cp ec
    return res
#endif

newtype Maintainer = Maintainer { unMaintainer :: Text }
    deriving (Show, Eq, Ord, Hashable, ToJSON, FromJSON, IsString)

-- | Name of an executable.
newtype ExeName = ExeName { unExeName :: Text }
    deriving (Show, Eq, Ord, Hashable, ToJSON, FromJSON, IsString)

intersectVersionRanges :: VersionRange -> VersionRange -> VersionRange
intersectVersionRanges x y = C.simplifyVersionRange $ C.intersectVersionRanges x y

-- | There seems to be a bug in Cabal where serializing and deserializing
-- version ranges winds up with different representations. So we have a
-- super-simplifier to deal with that.
simplifyVersionRange :: VersionRange -> VersionRange
simplifyVersionRange vr =
    fromMaybe (assert False vr') $ simpleParse $ display vr'
  where
    vr' = C.simplifyVersionRange vr

-- | Topologically sort so that items with dependencies occur after those
-- dependencies.
topologicalSort :: (Ord key, Show key, MonadThrow m, Typeable key)
                => (value -> finalValue)
                -> (value -> Set key) -- ^ deps
                -> Map key value
                -> m (Vector (key, finalValue))
topologicalSort toFinal toDeps =
    loop id . mapWithKey removeSelfDeps . fmap (toDeps &&& toFinal)
  where
    removeSelfDeps k (deps, final) = (deleteSet k deps, final)
    loop front toProcess | null toProcess = return $ pack $ front []
    loop front toProcess
        | null noDeps = throwM $ NoEmptyDeps (map fst toProcess')
        | otherwise = loop (front . noDeps') (mapFromList hasDeps)
      where
        toProcess' = fmap (first removeUnavailable) toProcess
        allKeys = Map.keysSet toProcess
        removeUnavailable = asSet . setFromList . filter (`member` allKeys) . setToList
        (noDeps, hasDeps) = partition (null . fst . snd) $ mapToList toProcess'
        noDeps' = (map (second snd) noDeps ++)

data TopologicalSortException key = NoEmptyDeps (Map key (Set key))
    deriving (Show, Typeable)
instance (Show key, Typeable key) => Exception (TopologicalSortException key)
