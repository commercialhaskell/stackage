{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Get the proposed build plan.
module Stackage2.ProposedPlan
    ( ProposedPlan (..)
    , defaultProposedPlan
    ) where

import           Stackage2.Prelude
import qualified Stackage.Config as Old
import qualified Stackage.Types  as Old

data ProposedPlan = ProposedPlan
    { ppPackages :: Map PackageName (VersionRange, Maintainer)
    -- ^ This does not include core packages or dependencies, just packages
    -- added by some maintainer.
    , ppExpectedFailures :: Set PackageName
    -- ^ At some point in the future, we should split this into Haddock
    -- failures, test failures, etc.
    }

-- | The proposed plan from the requirements provided by contributors.
defaultProposedPlan :: ProposedPlan
defaultProposedPlan = ProposedPlan
    { ppPackages = fmap (Maintainer . pack . Old.unMaintainer)
               <$> Old.defaultStablePackages ghcVer False
    , ppExpectedFailures = Old.defaultExpectedFailures ghcVer False
    }
  where
    ghcVer = Old.GhcMajorVersion 7 8
