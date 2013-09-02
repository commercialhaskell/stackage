{-# LANGUAGE CPP #-}
module Stackage.Config where

import           Control.Monad              (when)
import           Control.Monad.Trans.Writer (execWriter, tell)
import qualified Data.Map                   as Map
import           Data.Set                   (fromList)
import           Distribution.Text          (simpleParse)
import           Stackage.Types

-- | Packages which are shipped with GHC but are not included in the
-- Haskell Platform list of core packages.
defaultExtraCore :: GhcMajorVersion -> Set PackageName
defaultExtraCore _ = fromList $ map PackageName $ words
    "binary Win32"

-- | Test suites which are expected to fail for some reason. The test suite
-- will still be run and logs kept, but a failure will not indicate an
-- error in our package combination.
defaultExpectedFailures :: GhcMajorVersion
                        -> Set PackageName
defaultExpectedFailures _ = fromList $ map PackageName
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

      -- https://github.com/vincenthz/language-java/issues/10
    , "language-java"

    , "threads"
    , "crypto-conduit"
    , "pandoc"
    , "language-ecmascript"
    , "hspec"
    , "alex"

      -- https://github.com/basvandijk/concurrent-extra/issues/
    , "concurrent-extra"

      -- https://github.com/rrnewton/haskell-lockfree-queue/issues/7
    , "abstract-deque"

      -- https://github.com/skogsbaer/xmlgen/issues/2
    , "xmlgen"

      -- Something very strange going on with the test suite, I can't figure
      -- out how to fix it
    , "bson"
    ]

-- | List of packages for our stable Hackage. All dependencies will be
-- included as well. Please indicate who will be maintaining the package
-- via comments.
defaultStablePackages :: GhcMajorVersion -> Map PackageName (VersionRange, Maintainer)
defaultStablePackages ghcVer = unPackageMap $ execWriter $ do
    mapM_ (add "michael@snoyman.com") $ words =<<
        [ "yesod yesod-newsfeed yesod-sitemap yesod-static yesod-test yesod-bin"
        , "markdown filesystem-conduit mime-mail-ses"
        , "persistent persistent-template persistent-sqlite"
        , "network-conduit-tls yackage warp-tls keter"
        , "shakespeare-text process-conduit stm-conduit"
        , "classy-prelude-yesod yesod-fay"
        , "random-shuffle safe-failure"
        ]

    mapM_ (add "FP Complete <michael@fpcomplete.com>") $ words =<<
        [ "web-fpco th-expand-syns configurator compdata smtLib unification-fd"
        , "fixed-list indents language-c pretty-class"
        , "aws yesod-auth-oauth csv-conduit cassava"
        , "async shelly"
        , "hxt dimensional"
        , "cairo diagrams-cairo"
        , "persistent-mongoDB"
        ]
    when (ghcVer < GhcMajorVersion 7 6) $ do
        addRange "FP Complete <michael@fpcomplete.com>" "hxt" "<= 9.3.0.1"
        addRange "FP Complete <michael@fpcomplete.com>" "shelly" "<= 1.0"
    when (ghcVer >= GhcMajorVersion 7 6) $ do
        add "FP Complete <michael@fpcomplete.com>" "repa-devil"
    addRange "FP Complete <michael@fpcomplete.com>" "kure" "<= 2.4.10"

    mapM_ (add "Neil Mitchell") $ words
        "hlint hoogle shake derive"

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
        , "tls-debug tls-extra vhd"
        ]
    addRange "Vincent Hanquez" "language-java" "< 0.2.5"

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
    -- Does not compile on Windows
    mapM_ (add "Vincent Hanquez") $ words "udbus xenstore"
#endif

    mapM_ (add "Alberto G. Corona <agocorona@gmail.com>") $ words
         "RefSerialize TCache Workflow MFlow"

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
        , "diagrams-postscript diagrams-builder diagrams-haddock haxr"
        , "BlogLiterately BlogLiterately-diagrams"
        ]

    mapM_ (add "Patrick Brisbin") $ words "gravatar"

    mapM_ (add "Felipe Lessa <felipe.lessa@gmail.com>") $ words
        "esqueleto fb fb-persistent yesod-fb yesod-auth-fb"

    mapM_ (add "Alexander Altman <alexanderaltman@me.com>") $ words
        "base-unicode-symbols containers-unicode-symbols"

    mapM_ (add "Ryan Newton <ryan.newton@alum.mit.edu>") $ words
        "accelerate"

    mapM_ (add "Dan Burton <danburton.email@gmail.com>") $ words =<<
        [ "basic-prelude composition io-memoize numbers rev-state runmemo"
        , "tardis"
        ]

    mapM_ (add "Daniel Díaz <dhelta.diaz@gmail.com>") $ words
        "HaTeX"

    mapM_ (add "Adam Bergmark <adam@edea.se>") $ words
        "fay fay-base"

    -- https://github.com/fpco/stackage/issues/46
    addRange "Michael Snoyman" "QuickCheck" "< 2.6"

    -- https://github.com/fpco/stackage/issues/68
    addRange "Michael Snoyman" "criterion" "< 0.8"

    -- https://github.com/fpco/stackage/issues/72
    addRange "Michael Snoyman" "HaXml" "< 1.24"

    -- Due to binary package dep
    addRange "Michael Snoyman" "statistics" "< 0.10.4"

    -- https://github.com/fpco/stackage/issues/97
    addRange "Michael Snoyman" "tagsoup" "< 0.13"

    -- https://github.com/fpco/stackage/issues/107
    addRange "Michael Snoyman" "split" "< 0.2.2"

    addRange "Michael Snoyman" "hashable" "< 1.2"

    -- unknown symbol `utf8_table4'
    addRange "Michael Snoyman" "regex-pcre-builtin" "< 0.94.4.6.8.31"
  where
    add maintainer package = addRange maintainer package "-any"
    addRange maintainer package range =
        case simpleParse range of
            Nothing -> error $ "Invalid range " ++ show range ++ " for " ++ package
            Just range' -> tell $ PackageMap $ Map.singleton (PackageName package) (range', Maintainer maintainer)
