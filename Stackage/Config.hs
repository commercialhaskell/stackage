module Stackage.Config where

import qualified Data.Map as Map
import Stackage.Types
import Control.Monad.Trans.Writer (execWriter, tell)
import Data.Set (singleton, fromList)
import Control.Monad (when, unless)
import Distribution.System (OS (..), buildOS)
import           Distribution.Text    (simpleParse)

-- | Packages which are shipped with GHC but are not included in the
-- Haskell Platform list of core packages.
extraCore :: Set PackageName
extraCore = singleton $ PackageName "binary"

-- | Test suites which are expected to fail for some reason. The test suite
-- will still be run and logs kept, but a failure will not indicate an
-- error in our package combination.
expectedFailures :: Set PackageName
expectedFailures = fromList $ map PackageName
    [ -- Requires an old version of WAI and Warp for tests
      "HTTP"
      -- Requires a special hspec-meta which is not yet available from
      -- Hackage.
    , "hspec"
    ]

-- | List of packages for our stable Hackage. All dependencies will be
-- included as well. Please indicate who will be maintaining the package
-- via comments.
stablePackages :: Map PackageName VersionRange
stablePackages = execWriter $ do
    -- Michael Snoyman michael@snoyman.com
    addRange "yesod" "< 1.4"
    add "yesod-newsfeed"
    add "yesod-sitemap"
    add "yesod-static"
    -- A few transient deps not otherwise picked up
    add "cipher-aes"
    when (buildOS == Linux) $ add "hinotify"
    unless (buildOS == Windows) $ add "unix-time"
  where
    add = flip addRange "-any"
    addRange package range =
        case simpleParse range of
            Nothing -> error $ "Invalid range " ++ show range ++ " for " ++ package
            Just range' -> tell $ Map.singleton (PackageName package) range'
