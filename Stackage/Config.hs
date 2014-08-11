{-# LANGUAGE CPP #-}
module Stackage.Config where

import           Control.Monad              (when)
import           Control.Monad.Trans.Writer (execWriter, tell)
import           Data.Char                  (toLower)
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe)
import           Data.Set                   (fromList, singleton)
import           Distribution.Text          (simpleParse)
import           Stackage.Types

-- | Packages which are shipped with GHC but are not included in the
-- Haskell Platform list of core packages.
defaultExtraCore :: GhcMajorVersion -> Set PackageName
defaultExtraCore _ = fromList $ map PackageName $ words
    "binary Win32 ghc-prim integer-gmp"

-- | Test suites which are expected to fail for some reason. The test suite
-- will still be run and logs kept, but a failure will not indicate an
-- error in our package combination.
defaultExpectedFailures :: GhcMajorVersion
                        -> Set PackageName
defaultExpectedFailures ghcVer = execWriter $ do
      -- Requires an old version of WAI and Warp for tests
    add "HTTP"

      -- text and setenv have recursive dependencies in their tests, which
      -- cabal can't (yet) handle
    add "text"
    add "setenv"

      -- The version of GLUT included with the HP does not generate
      -- documentation correctly.
    add "GLUT"

      -- https://github.com/bos/statistics/issues/42
    add "statistics"

      -- https://github.com/kazu-yamamoto/simple-sendfile/pull/10
    add "simple-sendfile"

      -- http://hackage.haskell.org/trac/hackage/ticket/954
    add "diagrams"

      -- https://github.com/fpco/stackage/issues/24
    add "unix-time"

      -- With transformers 0.3, it doesn't provide any modules
    add "transformers-compat"

      -- Tests require shell script and are incompatible with sandboxed package
      -- databases
    add "HTF"

      -- https://github.com/simonmar/monad-par/issues/28
    add "monad-par"

      -- Unfortunately network failures seem to happen haphazardly
    add "network"

      -- https://github.com/ekmett/hyphenation/issues/1
    add "hyphenation"

      -- Test suite takes too long to run on some systems
    add "punycode"

      -- http://hub.darcs.net/stepcut/happstack/issue/1
    add "happstack-server"

      -- Requires a Facebook app.
    add "fb"

      -- https://github.com/tibbe/hashable/issues/64
    add "hashable"

      -- https://github.com/vincenthz/language-java/issues/10
    add "language-java"

    add "threads"
    add "crypto-conduit"
    add "pandoc"
    add "language-ecmascript"
    add "hspec"
    add "alex"

      -- https://github.com/basvandijk/concurrent-extra/issues/
    add "concurrent-extra"

      -- https://github.com/skogsbaer/xmlgen/issues/2
    add "xmlgen"

      -- Something very strange going on with the test suite, I can't figure
      -- out how to fix it
    add "bson"

      -- Requires a locally running PostgreSQL server with appropriate users
    add "postgresql-simple"

      -- Missing files
    add "websockets"

      -- Some kind of Cabal bug when trying to run tests
    add "thyme"

    when (ghcVer < GhcMajorVersion 7 6) $ do
        -- https://github.com/haskell-suite/haskell-names/issues/39
        add "haskell-names"

    add "shake"

    -- https://github.com/jgm/pandoc-citeproc/issues/5
    add "pandoc-citeproc"

    -- Problems with doctest and sandboxing
    add "warp"
    add "wai-logger"

    -- https://github.com/fpco/stackage/issues/163
    add "hTalos"
    add "seqloc"

    -- FIXME the test suite fails fairly regularly in builds, though I haven't
    -- discovered why yet
    add "crypto-numbers"

    -- Test suite is currently failing regularly, needs to be worked out still.
    add "lens"

    -- Requires too old a version of test-framework
    add "time"

    -- No code included any more, therefore Haddock fails
    mapM_ add $ words =<<
        [ "comonad-transformers comonads-fd groupoids"
        , "profunctor-extras semigroupoid-extras"
        , "hamlet shakespeare-css shakespeare-i18n"
        , "shakespeare-js shakespeare-text"
        , "attoparsec-conduit blaze-builder-conduit http-client-conduit"
        , "network-conduit zlib-conduit http-client-multipart"
        , "wai-eventsource wai-test"
        ]

    -- Cloud Haskell tests seem to be unreliable
    mapM_ add $ words =<<
        [ "distributed-process lockfree-queue network-transport-tcp"
        ]

    -- Pulls in monad-peel which does not compile
    when (ghcVer >= GhcMajorVersion 7 8) $ add "monad-control"

    -- issues with pthread
    mapM_ add $ words "hlibgit2 gitlib-s3 gitlib-libgit2"

    -- https://github.com/fpco/stackage/issues/226
    add "options"

    -- Requires too high a bytestring
    when (ghcVer <= GhcMajorVersion 7 4) $ add "scientific"

    -- https://github.com/gtk2hs/gtk2hs/issues/36
    add "glib"
    add "pango"

    -- https://github.com/acw/bytestring-progress/issues/3
    add "bytestring-progress"

    -- Seems to require 32-bit functions
    add "nettle"

    -- Depends on a missing graphviz executable
    add "graphviz"

    -- https://github.com/silkapp/json-schema/issues/8
    when (ghcVer <= GhcMajorVersion 7 6) $
        add "json-schema"

  where
    add = tell . singleton . PackageName

-- | List of packages for our stable Hackage. All dependencies will be
-- included as well. Please indicate who will be maintaining the package
-- via comments.
defaultStablePackages :: GhcMajorVersion
                      -> Bool -- ^ using haskell platform?
                      -> Map PackageName (VersionRange, Maintainer)
defaultStablePackages ghcVer requireHP = unPackageMap $ execWriter $ do
    mapM_ (add "michael@snoyman.com") $ words =<<
        [ "yesod yesod-newsfeed yesod-sitemap yesod-static yesod-test yesod-bin"
        , "markdown mime-mail-ses"
        , "persistent persistent-template persistent-sqlite"
        , "network-conduit-tls yackage warp-tls keter"
        , "process-conduit stm-conduit"
        , "classy-prelude-yesod yesod-fay yesod-eventsource wai-websockets"
        , "random-shuffle hebrew-time"
        , "bzlib-conduit case-insensitive"
        , "conduit-extra conduit-combinators yesod-websockets"
        ]

    -- https://github.com/fpco/stackage/issues/261
    addRange "Michael Snoyman" "cabal-install" $
        case () of
            ()
                | ghcVer <= GhcMajorVersion 7 4 -> "< 1.15"
                | ghcVer <= GhcMajorVersion 7 6 -> "< 1.17"
                | ghcVer <= GhcMajorVersion 7 8 -> "< 1.19"
                | otherwise -> "-any"

    when (ghcVer >= GhcMajorVersion 7 6) $ add "michael@snoyman.com" "cabal-src"
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
    -- Does not compile on Windows
    mapM_ (add "michael@snoyman.com") $ words "judy"
#endif

    mapM_ (add "FP Complete <michael@fpcomplete.com>") $ words =<<
        [ "web-fpco th-expand-syns configurator smtLib"
        , "fixed-list indents language-c pretty-class"
        , "aws csv-conduit cassava"
        , "async shelly thyme"
        , "hxt hxt-relaxng dimensional"
        , "cairo diagrams-cairo gtk2hs-buildtools"
        , "persistent-mongoDB fpco-api"
        , "base16-bytestring convertible"
        , "compdata hybrid-vectors"
        , "executable-path formatting quandl-api"
        ]
    when (ghcVer < GhcMajorVersion 7 8) $ do -- No GHC 7.8 support
        mapM_ (add "FP Complete <michael@fpcomplete.com>") $ words =<<
            [ "" -- too unreliable for the moment "distributed-process distributed-process-simplelocalnet"
            , "threepenny-gui unification-fd"
            ]
        addRange "FP Complete <michael@fpcomplete.com>" "compdata" "< 0.8"
    when (ghcVer >= GhcMajorVersion 7 8) $
        mapM_ (add "FP Complete <michael@fpcomplete.com>") $ words =<<
            [ "criterion"
            ]

    -- Deprecated version
    addRange "FP Complete <michael@fpcomplete.com>" "persistent-mongoDB" "< 1.3.1 || > 1.3.1"
    when (ghcVer < GhcMajorVersion 7 6) $ do
        when requireHP $ do
            addRange "FP Complete <michael@fpcomplete.com>" "hxt" "<= 9.3.0.1"
        addRange "FP Complete <michael@fpcomplete.com>" "shelly" "<= 1.0"
        addRange "FP Complete <michael@fpcomplete.com>" "lockfree-queue" "== 0.2"
    -- when (ghcVer == GhcMajorVersion 7 6) $ do -- No GHC 7.8 support
    --     add "FP Complete <michael@fpcomplete.com>" "repa-devil"
    addRange "FP Complete <michael@fpcomplete.com>" "kure" "<= 2.4.10"

    mapM_ (add "Neil Mitchell") $ words
        "hlint hoogle shake derive tagsoup cmdargs safe uniplate nsis"

    mapM_ (add "Alan Zimmerman") $ words
        "hjsmin language-javascript"
    -- Requires newer alex than provided with HP
    when (ghcVer == GhcMajorVersion 7 4 && requireHP) $
        addRange "Alan Zimmerman" "language-javascript" "== 0.5.9"

    mapM_ (add "Jasper Van der Jeugt") $ words
        "blaze-html blaze-markup stylish-haskell"

    mapM_ (add "Antoine Latter") $ words
        "uuid byteorder"

    mapM_ (add "Stefan Wehr <wehr@factisresearch.com>") $ words
        "HTF xmlgen stm-stats"
    when (ghcVer < GhcMajorVersion 7 8) $ add "Stefan Wehr <wehr@factisresearch.com>" "hscurses"

    mapM_ (add "Bart Massey <bart.massey+stackage@gmail.com>") $ words
        "parseargs"

    mapM_ (add "Vincent Hanquez") $ words =<<
        [ "bytedump certificate cipher-aes cipher-rc4 connection"
        , "cprng-aes cpu crypto-pubkey-types crypto-random-api cryptocipher"
        , "cryptohash hit language-java libgit pem siphash socks tls"
        , "tls-debug vhd language-java"
        ]

    mapM_ (add "Chris Done") $ words
        "statistics-linreg"

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
    -- Does not compile on Windows
    mapM_ (add "Vincent Hanquez") $ words "udbus xenstore"
#endif

    when (ghcVer < GhcMajorVersion 7 8) $
        mapM_ (add "Alberto G. Corona <agocorona@gmail.com>") $ words
             "RefSerialize TCache Workflow MFlow"

    mapM_ (add "Edward Kmett <ekmett@gmail.com>") $ words =<<
        [ "ad adjunctions bifunctors bound charset comonad comonad-transformers"
        , "comonads-fd compressed concurrent-supply constraints contravariant"
        , "distributive either eq free groupoids heaps hyphenation"
        , "integration intervals kan-extensions lca lens linear monadic-arrays machines"
        , "mtl profunctors profunctor-extras reducers reflection"
        , "semigroups semigroupoids semigroupoid-extras speculation tagged void"
        , "graphs monad-products monad-st wl-pprint-extras wl-pprint-terminfo"
        , "numeric-extras parsers pointed prelude-extras reducers"
        , "streams vector-instances"
        ]
    when (ghcVer < GhcMajorVersion 7 8) $
        mapM_ (add "Edward Kmett <ekmett@gmail.com>") $ words =<<
            [ "categories comonad-extras recursion-schemes syb-extras"
            ]

    when (ghcVer >= GhcMajorVersion 7 6) $
        mapM_ (add "Andrew Farmer <afarmer@ittc.ku.edu>") $ words
            "scotty wai-middleware-static"

    mapM_ (add "Simon Hengel <sol@typeful.net>") $ words
        "hspec doctest base-compat"

    mapM_ (add "Mario Blazevic <blamario@yahoo.com>") $ words
        "monad-parallel monad-coroutine"
    -- https://github.com/blamario/monoid-subclasses/issues/3
    when (ghcVer >= GhcMajorVersion 7 6) $ do
        mapM_ (add "Mario Blazevic <blamario@yahoo.com>") $ words
            "incremental-parser monoid-subclasses"

    mapM_ (add "Brent Yorgey <byorgey@gmail.com>") $ words =<<
        [ "monoid-extras dual-tree vector-space-points active force-layout"
        , "diagrams diagrams-contrib diagrams-core diagrams-lib diagrams-svg"
        , "diagrams-postscript diagrams-builder diagrams-haddock haxr"
        , "BlogLiterately BlogLiterately-diagrams"
        , "MonadRandom"
        ]
    mapM_ (add "Vincent Berthoux <vincent.berthoux@gmail.com>") $ words
       "JuicyPixels"

    mapM_ (add "Patrick Brisbin") $ words "gravatar"

    mapM_ (add "Felipe Lessa <felipe.lessa@gmail.com>") $ words
        "esqueleto fb fb-persistent yesod-fb yesod-auth-fb"

    mapM_ (add "Alexander Altman <alexanderaltman@me.com>") $ words
        "base-unicode-symbols containers-unicode-symbols"

    -- NOTE: accelerate not yet supported on GHC 7.8
    when (ghcVer == GhcMajorVersion 7 6) $ do
        mapM_ (add "Ryan Newton <ryan.newton@alum.mit.edu>") $ words
            "accelerate"
    when (ghcVer < GhcMajorVersion 7 6) $ do
        addRange "Ryan Newton <ryan.newton@alum.mit.edu>" "accelerate" "< 0.14"
        addRange "Ryan Newton <ryan.newton@alum.mit.edu>" "fclabels" "< 2.0"

    mapM_ (add "Dan Burton <danburton.email@gmail.com>") $ words =<<
        [ "basic-prelude composition io-memoize numbers rev-state runmemo"
        , "tardis lens-family-th"
        ]

    mapM_ (add "Daniel Díaz <dhelta.diaz@gmail.com>") $ words
        "HaTeX matrix"

    mapM_ (add "Gabriel Gonzalez <Gabriel439@gmail.com>")
        ["pipes", "pipes-parse", "pipes-concurrency"]

    mapM_ (add "Adam Bergmark <adam@bergmark.nl>") $ words
        "fay fay-base fay-dom fay-jquery fay-text fay-uri snaplet-fay"

    mapM_ (add "Boris Lykah <lykahb@gmail.com>") $ words
        "groundhog groundhog-th groundhog-sqlite groundhog-postgresql groundhog-mysql"

    mapM_ (add "Janne Hellsten <jjhellst@gmail.com>") $ words
        "sqlite-simple"

    mapM_ (add "Michal J. Gajda") $ words
        "iterable Octree FenwickTree hPDB"
    when (ghcVer == GhcMajorVersion 7 6) $ do -- No GHC 7.8 support
        mapM_ (add "Michal J. Gajda") $ words
            "hPDB-examples"

    mapM_ (add "Roman Cheplyaka <roma@ro-che.info>") $ words =<<
        [ "smallcheck tasty tasty-smallcheck tasty-quickcheck tasty-hunit tasty-golden"
        , "traverse-with-class regex-applicative time-lens"
        , "haskell-names haskell-packages hse-cpp"
        ]

    mapM_ (add "George Giorgidze <giorgidze@gmail.com>") $ words
        "HCodecs YampaSynth"

    when (ghcVer >= GhcMajorVersion 7 6) $ do -- No GHC 7.4 support
        mapM_ (add "Phil Hargett <phil@haphazardhouse.net>") $ words
            "courier"

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
    mapM_ (add "Aycan iRiCAN <iricanaycan@gmail.com>") $ words
        "hdaemonize hsyslog hweblib"
#else
    mapM_ (add "Aycan iRiCAN <iricanaycan@gmail.com>") $ words
        "hweblib"
#endif

    mapM_ (add "Joachim Breitner <mail@joachim-breitner.de>") $ words
        "circle-packing arbtt"

    when (ghcVer < GhcMajorVersion 7 8) $
        mapM_ (add "John Wiegley") $ words =<<
            -- Removed these: bad semigroups upper bound
            [ "bindings-DSL github monad-extras numbers hlibgit2"
            , "gitlib gitlib-cmdline gitlib-test"
            , "gitlib-libgit2"
            -- https://github.com/jwiegley/gitlib/issues/31
            , "gitlib-s3"
            ]

    mapM_ (add "Ben Ford <ben@dlstartup.com") $ words
        "HandsomeSoup"

    mapM_ (add "Clint Adams <clint@debian.org>") $ words
        "DAV hOpenPGP hopenpgp-tools openpgp-asciiarmor MusicBrainz"

    -- https://github.com/fpco/stackage/issues/160
    when (ghcVer >= GhcMajorVersion 7 6) $ do
      mapM_ (add "Ketil Malde") $ words =<<
        [ "biocore biofasta biofastq biosff"
        , "blastxml bioace biophd"
        , "biopsl" -- https://github.com/ingolia/SamTools/issues/3 samtools
        , "seqloc bioalign BlastHTTP"
        -- The following have out-of-date dependencies currently
        -- biostockholm memexml RNAwolf
        -- , "Biobase BiobaseDotP BiobaseFR3D BiobaseInfernal BiobaseMAF"
        -- , "BiobaseTrainingData BiobaseTurner BiobaseXNA BiobaseVienna"
        -- , "BiobaseTypes BiobaseFasta"
        -- MC-Fold-DP
        ]
      -- https://github.com/fpco/stackage/issues/163
      addRange "Michael Snoyman" "biophd" "< 0.0.6 || > 0.0.6"
    when (ghcVer == GhcMajorVersion 7 6) $ do
        mapM_ (add "Ketil Malde") $ words =<<
            -- RNAFold
            [ "hTalos parsestar"
            ]

    mapM_ (add "Silk <code@silk.co>") $ words =<<
      [ "aeson-utils arrow-list attoparsec-expr bumper code-builder fay-builder"
      , "generic-aeson hxt-pickle-utils json-schema multipart regular-xmlpickler"
      , "rest-client rest-core rest-gen rest-happstack rest-snap rest-stringmap"
      , "rest-types rest-wai tostring uri-encode imagesize-conduit"
      ]

    mapM_ (add "Simon Michael <simon@joyful.com>") $ words
        "hledger"

    -- Newest hxt requires network 2.4 or newest
    when (ghcVer == GhcMajorVersion 7 4 && requireHP) $ do
        addRange "Michael Snoyman" "hxt" "< 9.3.1"
        addRange "Michael Snoyman" "network" "< 2.4"

    -- https://github.com/fpco/stackage/issues/197
    when (ghcVer == GhcMajorVersion 7 6 && requireHP) $
        addRange "Michael Snoyman" "parsers" "< 0.11"

    -- https://github.com/fpco/stackage/issues/216
    -- QuickCheck constraint
    -- when (ghcVer == GhcMajorVersion 7 6) $
    --     addRange "Michael Snoyman" "repa" "< 3.2.5.1"

    -- https://github.com/fpco/stackage/issues/217
    addRange "Michael Snoyman" "transformers" "< 0.4"
    addRange "Michael Snoyman" "mtl" "< 2.2"
    addRange "Michael Snoyman" "lifted-base" "< 0.2.2.2"

    -- https://github.com/fpco/stackage/issues/224
    when (ghcVer <= GhcMajorVersion 7 6) $ do
        addRange "Michael Snoyman" "zip-archive" "== 0.2.2.1"

    -- https://github.com/fpco/stackage/issues/231
    addRange "Michael Snoyman" "optparse-applicative" "< 0.9"

    -- https://github.com/fpco/stackage/issues/242
    addRange "Michael Snoyman" "mongoDB" "< 1.6"

    -- https://github.com/jgm/pandoc/issues/1442
    addRange "Michael Snoyman" "texmath" "<= 0.6.6.3"

    -- Requires too new a version of text
    when (ghcVer == GhcMajorVersion 7 4 && requireHP) $ do
        addRange "Michael Snoyman" "attoparsec" "< 0.11.2.1"
        addRange "Michael Snoyman" "parsers" "< 0.11"

    -- local patch
    addRange "Michael Snoyman" "bson" "== 0.2.4"

    -- 0.16.2 fixes dependency issues with different version of GHC
    -- and Haskell Platform. Now builds on GHC 7.4-7.8. Version 1.0 is
    -- guaranteed to break the API. See
    -- https://travis-ci.org/jswebtools/language-ecmascript for
    -- current build status.
    addRange "Andrey Chudnov <oss@chudnov.com>" "language-ecmascript" ">= 0.16.2 && < 1.0"

    -- https://github.com/fpco/stackage/issues/270
    addRange "Michael Snoyman" "JuicyPixels" "< 3.1.6"

    -- https://github.com/fpco/stackage/issues/269
    addRange "Michael Snoyman" "tasty-hunit" "< 0.9"
  where
    add maintainer package = addRange maintainer package "-any"
    addRange maintainer package range =
        case simpleParse range of
            Nothing -> error $ "Invalid range " ++ show range ++ " for " ++ package
            Just range' -> tell $ PackageMap $ Map.singleton (PackageName package) (range', Maintainer maintainer)

-- | Replacement Github users. This is useful when a project is owned by an
-- organization. It also lets you ping multiple users.
--
-- Note that cross organization team mentions aren't allowed by Github.
convertGithubUser :: String -> [String]
convertGithubUser x =
    fromMaybe [x] $ Map.lookup (map toLower x) pairs
  where
    pairs = Map.fromList
        [ ("diagrams",     ["byorgey", "fryguybob", "jeffreyrosenbluth", "bergey"])
        , ("yesodweb",     ["snoyberg"])
        , ("fpco",         ["snoyberg"])
        , ("faylang",      ["bergmark"])
        , ("silkapp",      ["bergmark"])
        , ("snapframework",["mightybyte"])
        ]
