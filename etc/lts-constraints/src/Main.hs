{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Main (main) where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (MonadState (..), runStateT)
import Data.Text (Text)
import Options.Generic (getRecord, ParseRecord)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.Generics (Generic)
import RIO.Map (Map)
import System.IO (openFile, IOMode (..), hFlush, hClose)

import BuildConstraints (parsePackageDecl, handlePackage)
import Snapshot (snapshotMap, loadSnapshot)
import Types (PackageName, Version)

src :: String
src = "../../build-constraints.yaml"

target :: Int -> String
target major = "lts-" <> show major <> "-build-constraints.yaml"

data Args = Args
  { major :: Int
  , baseSnapshotPath :: FilePath
  } deriving Generic

instance ParseRecord Args

data State
  = LookingForLibBounds
  | ProcessingLibBounds
  | Done

main :: IO ()
main = do
  Args { major, baseSnapshotPath } <- getRecord "lts-constraints"
  map <- snapshotMap <$> loadSnapshot baseSnapshotPath
  output <- openFile (target major) WriteMode
  let putLine = liftIO . T.hPutStrLn output
  lines <- T.lines <$> T.readFile src
  void $ flip runStateT LookingForLibBounds $ do
    forM_ lines $ putLine <=< processLine map
  hFlush output
  hClose output
  putStrLn $ "Done. Wrote to " <> target major

processLine :: MonadState State m => Map PackageName Version -> Text -> m Text
processLine map line = do
  st <- get
  case st of
    LookingForLibBounds -> do
      when (line == "packages:") $
        put ProcessingLibBounds
      pure line
    ProcessingLibBounds ->
      if line == "# end of packages"
      then do
        put Done
        pure line
      else
        case parsePackageDecl line of
          Just p -> pure $ handlePackage map p
          Nothing -> pure line
    Done -> pure line
