{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Snapshot (loadSnapshot, snapshotMap) where

import Control.Arrow
import Data.Aeson
import GHC.Generics
import RIO.Map (Map)
import qualified Data.Text as T
import qualified Data.Yaml as Y
import qualified RIO.Map as M

import Types

data Snapshot = Snapshot
  { packages :: [SnapshotPackage]
  } deriving (FromJSON, Generic, Show)

data SnapshotPackage = SnapshotPackage
  { hackage :: PackageVersion
  } deriving (FromJSON, Generic, Show)

data PackageVersion = PackageVersion
  { pvPackage :: PackageName
  , pvVersion :: Version
  } deriving Show

instance FromJSON PackageVersion where
  parseJSON s0 = do
    s1 <- parseJSON s0
    let s2 = T.takeWhile (/= '@') s1
    let xs = T.splitOn "-" s2
    pvPackage <- parseJSON $ String $ T.intercalate "-" (init xs)
    pvVersion <- parseJSON $ String $ last xs
    pure PackageVersion { pvPackage, pvVersion }

snapshotMap :: Snapshot -> Map PackageName Version
snapshotMap = M.fromList . map ((pvPackage &&& pvVersion) . hackage) . packages

loadSnapshot :: FilePath -> IO Snapshot
loadSnapshot = fmap (either (error . show) id) . Y.decodeFileEither
