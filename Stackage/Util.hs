module Stackage.Util where

import qualified Data.Map             as Map
import qualified Data.Set             as Set
import           Distribution.Version (thisVersion)
import           Stackage.Types

identsToRanges :: Set PackageIdentifier -> Map PackageName VersionRange
identsToRanges =
    Map.unions . map go . Set.toList
  where
    go (PackageIdentifier package version) = Map.singleton package $ thisVersion version
