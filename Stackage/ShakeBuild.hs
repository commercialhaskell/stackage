-- |

module Stackage.ShakeBuild where

import Data.Text (Text)
import Stackage.PerformBuild (PerformBuild(..))

-- | Run the shake builder.
performBuild :: PerformBuild -> IO [Text]
performBuild = undefined
