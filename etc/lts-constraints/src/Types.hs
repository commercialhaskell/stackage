{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Types where

import Control.Monad
import Data.Aeson
import Data.String.Conversions.Monomorphic
import Distribution.Text (simpleParse)
import Distribution.Types.VersionRange (VersionRange)
import GHC.Generics
import RIO.Text (Text)
import qualified Distribution.Types.PackageName as C (PackageName, mkPackageName)
import qualified Distribution.Types.Version as C (Version)

newtype PackageName = PackageName { unPackageName :: C.PackageName }
  deriving (Eq, Generic, Ord, FromJSONKey, Show)

mkPackageName :: Text -> PackageName
mkPackageName = PackageName . C.mkPackageName . fromStrictText

instance FromJSON PackageName where
  parseJSON = fmap (PackageName . C.mkPackageName) . parseJSON

newtype Version = Version { unVersion :: C.Version }
  deriving (Generic, Show)

instance FromJSON Version where
  parseJSON =
    maybe (fail "Invalid Version") (pure . Version) . simpleParse <=< parseJSON


data PackageDecl = PackageDecl
  { prefix :: Text
  , package :: PackageName
  , range :: VersionRange
  , suffix :: Text
  }
