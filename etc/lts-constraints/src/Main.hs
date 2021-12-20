{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Main (main) where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (MonadState (..), runStateT)
import Data.Text (Text)
import RIO.Map (Map)
import System.IO (openFile, IOMode (..), hFlush, hClose)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import BuildConstraints (parsePackageDecl, handlePackage)
import Snapshot (snapshotMap, loadSnapshot)
import Types (PackageName, Version)

src :: String
src = "../../build-constraints.yaml"

target :: String
target = "../../lts-build-constraints.yaml"

data State
  = LookingForLibBounds
  | ProcessingLibBounds
  | Done

main :: IO ()
main = do
  map <- snapshotMap <$> loadSnapshot "../../nightly-2012-12-11.yaml"
  output <- openFile target WriteMode
  let putLine = liftIO . T.hPutStrLn output
  lines <- T.lines <$> T.readFile src
  void $ flip runStateT LookingForLibBounds $ do
    forM_ lines $ putLine <=< processLine map
  hFlush output
  hClose output

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
