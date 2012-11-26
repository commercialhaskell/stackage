{-# LANGUAGE CPP #-}
module Stackage.Config where

import           Control.Monad.Trans.Writer (execWriter, tell)
import qualified Data.Map                   as Map
import           Data.Set                   (fromList, singleton)
import           Distribution.Text          (simpleParse)
import           Stackage.Types

targetCompilerVersion :: Version
targetCompilerVersion =
    case simpleParse "7.4.2" of
        Nothing -> error "Invalid targetCompilerVersion"
        Just v -> v

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

      -- text and setenv have recursive dependencies in their tests, which
      -- cabal can't (yet) handle
    , "text"
    , "setenv"

      -- The version of GLUT included with the HP does not generate
      -- documentation correctly.
    , "GLUT"
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
    add "yesod-test"
    add "markdown"
    add "filesystem-conduit"
    add "mime-mail-ses"

    -- Neil Mitchell
    add "hoogle"
    add "hlint"

    -- Alan Zimmerman
    add "hjsmin"
    add "language-javascript"

    -- Jasper Van der Jeugt
    add "blaze-html"
    add "blaze-markup"
    add "stylish-haskell"

    -- Antoine Latter
    add "uuid"
    add "byteorder"
  where
    add = flip addRange "-any"
    addRange package range =
        case simpleParse range of
            Nothing -> error $ "Invalid range " ++ show range ++ " for " ++ package
            Just range' -> tell $ Map.singleton (PackageName package) range'

verbose :: Bool
verbose =
#if VERBOSE
    True
#else
    False
#endif
