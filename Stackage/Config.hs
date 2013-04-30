{-# LANGUAGE CPP #-}
module Stackage.Config where

import           Control.Monad.Trans.Writer (execWriter, tell)
import qualified Data.Map                   as Map
import           Data.Set                   (fromList)
import           Distribution.Text          (simpleParse)
import           Stackage.Types

targetCompilerVersion :: Version
targetCompilerVersion =
    case simpleParse "7.4.2" of
        Nothing -> error "Invalid targetCompilerVersion"
        Just v -> v

-- | Packages which are shipped with GHC but are not included in the
-- Haskell Platform list of core packages.
defaultExtraCore :: Set PackageName
defaultExtraCore = fromList $ map PackageName $ words
    "binary Win32"

-- | Test suites which are expected to fail for some reason. The test suite
-- will still be run and logs kept, but a failure will not indicate an
-- error in our package combination.
defaultExpectedFailures :: Set PackageName
defaultExpectedFailures = fromList $ map PackageName
    [ -- Requires an old version of WAI and Warp for tests
      "HTTP"

      -- text and setenv have recursive dependencies in their tests, which
      -- cabal can't (yet) handle
    , "text"
    , "setenv"

      -- The version of GLUT included with the HP does not generate
      -- documentation correctly.
    , "GLUT"

      -- https://github.com/bos/statistics/issues/42
    , "statistics"

      -- https://github.com/kazu-yamamoto/simple-sendfile/pull/10
    , "simple-sendfile"

      -- http://hackage.haskell.org/trac/hackage/ticket/954
    , "diagrams"

      -- https://github.com/fpco/stackage/issues/24
    , "unix-time"

      -- With transformers 0.3, it doesn't provide any modules
    , "transformers-compat"

      -- Tests require shell script and are incompatible with sandboxed package
      -- databases
    , "HTF"

      -- https://github.com/simonmar/monad-par/issues/28
    , "monad-par"

      -- Unfortunately network failures seem to happen haphazardly
    , "network"

      -- https://github.com/ekmett/hyphenation/issues/1
    , "hyphenation"

      -- Test suite takes too long to run on some systems
    , "punycode"

      -- http://hub.darcs.net/stepcut/happstack/issue/1
    , "happstack-server"

      -- Requires a Facebook app.
    , "fb"

      -- https://github.com/tibbe/hashable/issues/64
    , "hashable"
    ]

-- | List of packages for our stable Hackage. All dependencies will be
-- included as well. Please indicate who will be maintaining the package
-- via comments.
defaultStablePackages :: Map PackageName (VersionRange, Maintainer)
defaultStablePackages = unPackageMap $ execWriter $ do
    mapM_ (add "michael@snoyman.com") $ words =<<
        [ "yesod yesod-newsfeed yesod-sitemap yesod-static yesod-test"
        , "markdown filesystem-conduit mime-mail-ses"
        , "persistent persistent-template persistent-sqlite"
        , "network-conduit-tls yackage" -- Removed until keter supports non-Linux systems: keter
        , "web-fpco"
        ]

    mapM_ (add "Neil Mitchell") $ words
        "hlint hoogle shake"

    mapM_ (add "Alan Zimmerman") $ words
        "hjsmin language-javascript"

    mapM_ (add "Jasper Van der Jeugt") $ words
        "blaze-html blaze-markup stylish-haskell"

    mapM_ (add "Antoine Latter") $ words
        "uuid byteorder"

    mapM_ (add "Stefan Wehr <wehr@factisresearch.com>") $ words
        "HTF hscurses xmlgen stm-stats"

    mapM_ (add "Bart Massey <bart.massey+stackage@gmail.com>") $ words
        "parseargs"

    mapM_ (add "Vincent Hanquez") $ words =<<
        [ "asn1-data bytedump certificate cipher-aes cipher-rc4 connection"
        , "cprng-aes cpu crypto-pubkey-types crypto-random-api cryptocipher"
        , "cryptohash hit language-java libgit pem siphash socks tls"
        , "tls-debug tls-extra udbus vhd xenstore"
        ]

    mapM_ (add "Edward Kmett <ekmett@gmail.com>") $ words =<<
        [ "ad adjunctions bifunctors bound categories charset comonad comonad-transformers"
        , "comonads-fd comonad-extras compressed concurrent-supply constraints contravariant"
        , "distributive either eq free groupoids heaps hyphenation"
        , "integration intervals kan-extensions lca lens linear monadic-arrays machines"
        , "mtl profunctors profunctor-extras reducers reflection"
        , "representable-functors representable-tries"
        , "semigroups semigroupoids semigroupoid-extras speculation tagged void"
        , "graphs monad-products monad-st wl-pprint-extras wl-pprint-terminfo"
        , "numeric-extras parsers pointed prelude-extras recursion-schemes reducers"
        , "streams syb-extras vector-instances"
        ]

    mapM_ (add "Simon Hengel <sol@typeful.net>") $ words
        "hspec doctest base-compat"

    mapM_ (add "Brent Yorgey <byorgey@gmail.com>") $ words =<<
        [ "monoid-extras dual-tree vector-space-points active force-layout"
        , "diagrams diagrams-contrib diagrams-core diagrams-lib diagrams-svg"
        , "diagrams-postscript diagrams-builder haxr"
        , "BlogLiterately BlogLiterately-diagrams"
        ]

    mapM_ (add "Patrick Brisbin") $ words "gravatar"

    mapM_ (add "Felipe Lessa <felipe.lessa@gmail.com>") $ words
        "esqueleto fb fb-persistent yesod-fb yesod-auth-fb"

    mapM_ (add "Alexander Altman <alexanderaltman@me.com>") $ words
        "base-unicode-symbols containers-unicode-symbols"

    -- https://github.com/fpco/stackage/issues/46
    addRange "Michael Snoyman" "QuickCheck" "< 2.6"
    addRange "Michael Snoyman" "syb" "< 0.4"

    -- https://github.com/fpco/stackage/issues/65
    addRange "Michael Snoyman" "tagged" "< 0.5"

    -- https://github.com/fpco/stackage/issues/63
    addRange "Michael Snoyman" "NumInstances" "< 1.3"

    -- https://github.com/fpco/stackage/issues/66
    addRange "Michael Snoyman" "persistent" "< 1.2"
    addRange "Michael Snoyman" "persistent-sqlite" "< 1.2"
    addRange "Michael Snoyman" "persistent-template" "< 1.2"

    addRange "Michael Snoyman" "crypto-api" "< 0.12"
  where
    add maintainer package = addRange maintainer package "-any"
    addRange maintainer package range =
        case simpleParse range of
            Nothing -> error $ "Invalid range " ++ show range ++ " for " ++ package
            Just range' -> tell $ PackageMap $ Map.singleton (PackageName package) (range', Maintainer maintainer)
