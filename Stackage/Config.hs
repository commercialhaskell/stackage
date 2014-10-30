{-# LANGUAGE CPP #-}
module Stackage.Config where

import           Control.Monad              (when, unless)
import           Control.Monad.Trans.Writer (Writer, execWriter, tell)
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
                        -> Bool -- ^ haskell platform
                        -> Set PackageName
defaultExpectedFailures ghcVer requireHP = execWriter $ do
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

    add "shake"

    -- https://github.com/jgm/pandoc-citeproc/issues/5
    add "pandoc-citeproc"

    -- Problems with doctest and sandboxing
    add "warp"
    add "wai-logger"

    -- https://github.com/fpco/stackage/issues/163
    add "hTalos"
    add "seqloc"

    -- https://github.com/bos/math-functions/issues/25
    add "math-functions"

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

    -- No AWS creds available
    add "aws"

    -- Not sure why...
    add "singletons"

    add "hspec2"
    add "hspec-wai"

    -- Requires too new a version of time
    when (ghcVer < GhcMajorVersion 7 8) $ add "cookie"

    -- https://github.com/fpco/stackage/issues/285
    add "diagrams-haddock"
    add "scientific"
    add "json-schema"

    -- https://github.com/BioHaskell/octree/issues/4
    add "Octree"

    -- No code until we upgrade to network 2.6
    add "network-uri"

    -- https://github.com/goldfirere/th-desugar/issues/12
    add "th-desugar"

    -- https://github.com/haskell/c2hs/issues/108
    add "c2hs"

    -- https://github.com/jmillikin/haskell-filesystem/issues/3
    add "system-filepath"

    -- For some unknown reason, doctest has trouble on GHC 7.6. This only
    -- happens during a Stackage test.
    --
    -- See: http://www.reddit.com/r/haskell/comments/2go92u/beginner_error_messages_in_c_vs_haskell/cklaspk
    when (ghcVer == GhcMajorVersion 7 6) $ add "http-types"

    -- Requires a running webdriver server
    add "webdriver"
    add "webdriver-snoy"

    -- Weird conflicts with sandboxing
    add "ghc-mod"
    add "ghcid"

    -- Requires locally running server
    add "bloodhound"

    when (ghcVer == GhcMajorVersion 7 8 && requireHP) $ do
        -- https://github.com/vincenthz/hs-asn1/issues/11
        add "asn1-encoding"

        -- https://github.com/vincenthz/hs-tls/issues/84
        add "tls"

        add "x509"
  where
    add = tell . singleton . PackageName

-- | List of packages for our stable Hackage. All dependencies will be
-- included as well. Please indicate who will be maintaining the package
-- via comments.
defaultStablePackages :: GhcMajorVersion
                      -> Bool -- ^ using haskell platform?
                      -> Map PackageName (VersionRange, Maintainer)
defaultStablePackages ghcVer requireHP = unPackageMap $ execWriter $ do
    when (ghcVer == GhcMajorVersion 7 8 && requireHP) haskellPlatform78
    mapM_ (add "michael@snoyman.com") $ words =<<
        [ "yesod yesod-newsfeed yesod-sitemap yesod-static yesod-test yesod-bin"
        , "markdown mime-mail-ses"
        , "persistent persistent-template persistent-sqlite persistent-postgresql persistent-mysql"
        , "network-conduit-tls yackage warp-tls keter"
        , "process-conduit stm-conduit"
        , "classy-prelude-yesod yesod-fay yesod-eventsource wai-websockets"
        , "random-shuffle hebrew-time"
        , "bzlib-conduit case-insensitive"
        , "conduit-extra conduit-combinators yesod-websockets"
        , "cabal-src"
        , "yesod-auth-deskcom monadcryptorandom sphinx"
        , "yesod-gitrepo"
        ]

    -- https://github.com/fpco/stackage/issues/261
    addRange "Michael Snoyman" "cabal-install" $
        case () of
            ()
                | ghcVer <= GhcMajorVersion 7 6 -> "< 1.17"
                | ghcVer <= GhcMajorVersion 7 8 -> "< 1.19"
                | otherwise -> "-any"

    mapM_ (add "FP Complete <michael@fpcomplete.com>") $ words =<<
        [ "web-fpco th-expand-syns configurator smtLib"
        , "fixed-list indents language-c pretty-class"
        , "csv-conduit cassava"
        , "async shelly thyme"
        , "hxt hxt-relaxng dimensional"
        , "cairo diagrams-cairo gtk2hs-buildtools"
        , "base16-bytestring convertible"
        , "compdata hybrid-vectors"
        , "executable-path formatting quandl-api"
        , "fgl hmatrix hmatrix-gsl"
        , "alex happy c2hs"
        , "fpco-api aws persistent-mongoDB"
        , "random-fu lhs2tex"
        , "Chart Chart-diagrams histogram-fill random-source"
        , "webdriver-snoy" -- Replace with webdriver after: https://github.com/kallisti-dev/hs-webdriver/issues/53
        -- https://github.com/Soostone/retry/pull/15
        -- , "retry"
        ]
    when (ghcVer < GhcMajorVersion 7 8) $ do -- No GHC 7.8 support
        mapM_ (add "FP Complete <michael@fpcomplete.com>") $ words =<<
            [ "" -- too unreliable for the moment "distributed-process distributed-process-simplelocalnet"
            -- https://github.com/fpco/stackage/issues/295
            --, "threepenny-gui unification-fd"
            ]
        addRange "FP Complete <michael@fpcomplete.com>" "compdata" "< 0.8"
    when (ghcVer >= GhcMajorVersion 7 8 && not requireHP) $
        mapM_ (add "FP Complete <michael@fpcomplete.com>") $ words =<<
            [ "criterion"
            , "th-lift singletons th-desugar quickcheck-assertions"
            ]

    addRange "FP Complete <michael@fpcomplete.com>" "kure" "<= 2.4.10"

    mapM_ (add "Neil Mitchell") $ words
        "hlint hoogle shake derive tagsoup cmdargs safe uniplate nsis js-jquery extra bake ghcid"

    mapM_ (add "Alan Zimmerman") $ words
        "hjsmin language-javascript"

    when (ghcVer >= GhcMajorVersion 7 8 && not requireHP) $
        mapM_ (add "Alfredo Di Napoli <alfredo.dinapoli@gmail.com>") $ words
            "mandrill"

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
    -- https://github.com/isomorphism/these/issues/11
    -- when (ghcVer >= GhcMajorVersion 7 8) $ add "Chris Done" "shell-conduit"

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

    mapM_ (add "Andrew Farmer <afarmer@ittc.ku.edu>") $ words
        "scotty wai-middleware-static"

    mapM_ (add "Simon Hengel <sol@typeful.net>") $ words
        "hspec doctest base-compat"

    mapM_ (add "Mario Blazevic <blamario@yahoo.com>") $ words
        "monad-parallel monad-coroutine incremental-parser monoid-subclasses"

    mapM_ (add "Brent Yorgey <byorgey@gmail.com>") $ words =<<
        [ "monoid-extras dual-tree vector-space-points active force-layout"
        , "diagrams diagrams-contrib diagrams-core diagrams-lib diagrams-svg"
        , "diagrams-postscript haxr"
        , "BlogLiterately"
        , "MonadRandom"
        -- #289: diagrams-builder diagrams-haddock BlogLiterately-diagrams
        ]
    mapM_ (add "Vincent Berthoux <vincent.berthoux@gmail.com>") $ words
       "JuicyPixels"

    mapM_ (add "Patrick Brisbin") $ words "gravatar"

    -- https://github.com/fpco/stackage/issues/299
    -- mapM_ (add "Paul Harper <benekastah@gmail.com>") $ words "yesod-auth-oauth2"

    mapM_ (add "Felipe Lessa <felipe.lessa@gmail.com>") $ words
        "esqueleto fb fb-persistent yesod-fb yesod-auth-fb"

    mapM_ (add "Alexander Altman <alexanderaltman@me.com>") $ words
        "base-unicode-symbols containers-unicode-symbols"

    if ghcVer >= GhcMajorVersion 7 8
        then add "Ryan Newton <ryan.newton@alum.mit.edu>" "accelerate"
        else addRange "Ryan Newton <ryan.newton@alum.mit.edu>" "accelerate" "< 0.15"

    mapM_ (add "Dan Burton <danburton.email@gmail.com>") $ words =<<
        [ "basic-prelude composition io-memoize numbers rev-state runmemo"
        , "tardis lens-family-th"
        ]

    mapM_ (add "Daniel Díaz <dhelta.diaz@gmail.com>") $ words
        "HaTeX matrix"
    when (ghcVer >= GhcMajorVersion 7 8) $
        mapM_ (add "Daniel Díaz <dhelta.diaz@gmail.com>") $ words
            "binary-list"

    mapM_ (add "Gabriel Gonzalez <Gabriel439@gmail.com>")
        ["pipes", "pipes-parse", "pipes-concurrency"]

    when (ghcVer >= GhcMajorVersion 7 8) $
        mapM_ (add "Chris Allen <cma@bitemyapp.com>")
            ["bloodhound"]

    mapM_ (add "Adam Bergmark <adam@bergmark.nl>") $ words
        "fay fay-base fay-dom fay-jquery fay-text fay-uri snaplet-fay"

    mapM_ (add "Boris Lykah <lykahb@gmail.com>") $ words
        "groundhog groundhog-th groundhog-sqlite groundhog-postgresql groundhog-mysql"

    mapM_ (add "Janne Hellsten <jjhellst@gmail.com>") $ words
        "sqlite-simple"

    mapM_ (add "Michal J. Gajda") $ words
        "iterable Octree FenwickTree"
    -- https://github.com/BioHaskell/hPDB/issues/2
    when (ghcVer >= GhcMajorVersion 7 8) $ do
        mapM_ (add "Michal J. Gajda") $ words
            "hPDB hPDB-examples"

    mapM_ (add "Roman Cheplyaka <roma@ro-che.info>") $ words =<<
        [ "smallcheck tasty tasty-smallcheck tasty-quickcheck tasty-hunit tasty-golden"
        , "traverse-with-class regex-applicative time-lens"
        , "haskell-names haskell-packages hse-cpp"
        ]

    mapM_ (add "George Giorgidze <giorgidze@gmail.com>") $ words
        "HCodecs YampaSynth"

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
    when (ghcVer >= GhcMajorVersion 7 8) $
        mapM_ (add "Joachim Breitner <mail@joachim-breitner.de>") $ words
            "ghc-heap-view"

    when (ghcVer < GhcMajorVersion 7 8) $
        mapM_ (add "John Wiegley") $ words =<<
            -- Removed these: bad semigroups upper bound
            [ "bindings-DSL github monad-extras numbers hlibgit2"
            , "gitlib gitlib-cmdline gitlib-test"
            , "gitlib-libgit2"
            -- https://github.com/jwiegley/gitlib/issues/31
            -- "gitlib-s3"
            ]

    mapM_ (add "Aditya Bhargava <adit@adit.io") $ words
        "HandsomeSoup"

    mapM_ (add "Clint Adams <clint@debian.org>") $ words
        "hOpenPGP openpgp-asciiarmor MusicBrainz DAV hopenpgp-tools"

    -- https://github.com/fpco/stackage/issues/160
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

    mapM_ (add "Silk <code@silk.co>") $ words =<<
      [ "arrow-list attoparsec-expr bumper code-builder fay-builder"
      , "hxt-pickle-utils multipart regular-xmlpickler"
      , "tostring uri-encode imagesize-conduit"
      ]
    when (ghcVer >= GhcMajorVersion 7 8 && not requireHP) $ do
        mapM_ (add "Silk <code@silk.co>") $ words =<<
              [ "aeson-utils generic-aeson json-schema"
              , "rest-client rest-core rest-gen rest-happstack rest-snap rest-stringmap"
              , "rest-types rest-wai tostring uri-encode imagesize-conduit"
              ]

    mapM_ (add "Simon Michael <simon@joyful.com>") $ words
        "hledger"

    mapM_ (add "Mihai Maruseac <mihai.maruseac@gmail.com>") $ words
        "io-manager"

    mapM_ (add "Dimitri Sabadie <dimitri.sabadie@gmail.com") $ words
        "monad-journal"

    mapM_ (add "Thomas Schilling <nominolo@googlemail.com>") $ words
        "ghc-syb-utils"

    mapM_ (add "Boris Buliga <d12frosted@icloud.com>") $ words
        "ghc-mod io-choice"
    when (ghcVer >= GhcMajorVersion 7 8) $
        mapM_ (add "Boris Buliga <d12frosted@icloud.com>") $ words
            "system-canonicalpath"

    when (ghcVer >= GhcMajorVersion 7 8) $
        mapM_ (add "Yann Esposito <yann.esposito@gmail.com>") $ words
            "holy-project"
    when requireHP $ addRange "Yann Esposito <yann.esposito@gmail.com>" "holy-project" "< 0.1.1.1"

    mapM_ (add "Paul Rouse <pgr@doynton.org>") $ words
        "yesod-auth-hashdb"

    add "Toralf Wittner <tw@dtex.org>" "zeromq4-haskell"

    mapM_ (add "trupill@gmail.com") $ words
        "djinn-lib djinn-ghc"

    mapM_ (add "Arash Rouhani <miffoljud@gmail.com>") $ words
        "yesod-text-markdown"

    -- https://github.com/fpco/stackage/issues/216
    -- QuickCheck constraint
    -- when (ghcVer == GhcMajorVersion 7 6) $
    --     addRange "Michael Snoyman" "repa" "< 3.2.5.1"

    -- https://github.com/fpco/stackage/issues/217
    addRange "Michael Snoyman" "transformers" "< 0.4"
    addRange "Michael Snoyman" "mtl" "< 2.2"
    addRange "Michael Snoyman" "lifted-base" "< 0.2.2.2"

    -- https://github.com/liyang/thyme/issues/29
    when (ghcVer <= GhcMajorVersion 7 6) $
        addRange "Michael Snoyman" "thyme" "< 0.3.5.3"

    -- https://github.com/fpco/stackage/issues/224
    when (ghcVer <= GhcMajorVersion 7 6) $ do
        addRange "Michael Snoyman" "zip-archive" "== 0.2.2.1"
        addRange "Michael Snoyman" "pandoc" "== 1.12.4.2"
        addRange "Michael Snoyman" "texmath" "<= 0.6.6.3"
        addRange "Michael Snoyman" "attoparsec" "== 0.11.3.1"
        addRange "Michael Snoyman" "parsers" "< 0.11"
        addRange "Michael Snoyman" "scientific" "< 0.3"
        addRange "Michael Snoyman" "aeson" "< 0.7.0.5"
        addRange "Michael Snoyman" "aeson-utils" "< 0.2.2"
        addRange "Michael Snoyman" "formatting" "< 5"
        addRange "Michael Snoyman" "aws" "< 0.10"

    -- 0.16.2 fixes dependency issues with different version of GHC
    -- and Haskell Platform. Now builds on GHC 7.4-7.8. Version 1.0 is
    -- guaranteed to break the API. See
    -- https://travis-ci.org/jswebtools/language-ecmascript for
    -- current build status.
    addRange "Andrey Chudnov <oss@chudnov.com>" "language-ecmascript" ">= 0.16.2 && < 1.0"

    -- https://github.com/fpco/stackage/issues/271
    when (ghcVer < GhcMajorVersion 7 8) $
        addRange "Michael Snoyman" "aeson" "< 0.8"

    -- https://github.com/fpco/stackage/issues/274
    addRange "Michael Snoyman" "pandoc-citeproc" "< 0.4"

    -- https://github.com/fpco/stackage/issues/276
    addRange "Michael Snoyman" "network" "< 2.6"
    addRange "Michael Snoyman" "network-uri" "< 2.6"

    -- https://github.com/fpco/stackage/issues/279
    addRange "Michael Snoyman" "MonadRandom" "< 0.2"

    -- https://github.com/fpco/stackage/issues/288
    addRange "Michael Snoyman" "text" "< 1.2"

    -- Force a specific version that's compatible with transformers 0.3
    addRange "Michael Snoyman" "transformers-compat" "== 0.3.3.3"

    -- https://github.com/fpco/stackage/issues/291
    addRange "Michael Snoyman" "random" "< 1.0.1.3"

    -- https://github.com/fpco/stackage/issues/314
    addRange "Michael Snoyman" "hxt" "< 9.3.1.9"

    -- https://github.com/fpco/stackage/issues/318
    addRange "Michael Snoyman" "HaXml" "< 1.25"

    -- https://github.com/fpco/stackage/issues/319
    addRange "Michael Snoyman" "polyparse" "< 1.10"

    -- https://github.com/fpco/stackage/issues/320
    addRange "Michael Snoyman" "lens" "< 4.5"

    when (ghcVer == GhcMajorVersion 7 8 && requireHP) $ do
        -- Yay workarounds for unnecessarily old versions
        let peg x y = addRange "Haskell Platform" x y
        peg "aeson" "== 0.7.0.4"
        peg "scientific" "== 0.2.0.2"
        peg "criterion" "<= 0.8.1.0"
        peg "tasty-quickcheck" "< 0.8.0.3"
        peg "formatting" "< 5.0"
        peg "parsers" "< 0.11"
        peg "lens" "< 4.2"
        peg "contravariant" "< 1"
        peg "adjunctions" "< 4.2"
        peg "kan-extensions" "< 4.1"
        peg "semigroupoids" "< 4.1"
        peg "aws" "< 0.10"
        peg "pandoc" "< 1.13"
        peg "texmath" "<= 0.6.6.3"
        peg "checkers" "== 0.3.2"
        peg "HandsomeSoup" "< 0.3.3"

add :: String -> String -> Writer PackageMap ()
add maintainer package = addRange maintainer package "-any"

addRange :: String -> String -> String -> Writer PackageMap ()
addRange maintainer package range =
    case simpleParse range of
        Nothing -> error $ "Invalid range " ++ show range ++ " for " ++ package
        Just range' -> tell $ PackageMap $ Map.singleton (PackageName package) (range', Maintainer maintainer)

-- | Hard coded Haskell Platform versions
haskellPlatform78 :: Writer PackageMap ()
haskellPlatform78 = do
    addRange "Haskell Platform" "ghc" "== 7.8.3"
    addRange "Haskell Platform" "haddock" "== 2.14.3"
    addRange "Haskell Platform" "array" "== 0.5.0.0"
    addRange "Haskell Platform" "base" "== 4.7.0.1"
    addRange "Haskell Platform" "bytestring" "== 0.10.4.0"
    addRange "Haskell Platform" "Cabal" "== 1.18.1.3"
    addRange "Haskell Platform" "containers" "== 0.5.5.1"
    addRange "Haskell Platform" "deepseq" "== 1.3.0.2"
    addRange "Haskell Platform" "directory" "== 1.2.1.0"
    addRange "Haskell Platform" "filepath" "== 1.3.0.2"
    addRange "Haskell Platform" "haskell2010" "== 1.1.2.0"
    addRange "Haskell Platform" "haskell98" "== 2.0.0.3"
    addRange "Haskell Platform" "hpc" "== 0.6.0.1"
    addRange "Haskell Platform" "old-locale" "== 1.0.0.6"
    addRange "Haskell Platform" "old-time" "== 1.1.0.2"
    addRange "Haskell Platform" "pretty" "== 1.1.1.1"
    addRange "Haskell Platform" "process" "== 1.2.0.0"
    addRange "Haskell Platform" "template-haskell" "== 2.9.0.0"
    addRange "Haskell Platform" "time" "== 1.4.2"
    addRange "Haskell Platform" "transformers" "== 0.3.0.0"
    addRange "Haskell Platform" "unix" "== 2.7.0.1"
    addRange "Haskell Platform" "xhtml" "== 3000.2.1"
    addRange "Haskell Platform" "async" "== 2.0.1.5"
    addRange "Haskell Platform" "attoparsec" "== 0.10.4.0"
    addRange "Haskell Platform" "case-insensitive" "== 1.1.0.3"
    addRange "Haskell Platform" "fgl" "== 5.5.0.1"
    addRange "Haskell Platform" "GLURaw" "== 1.4.0.1"
    addRange "Haskell Platform" "GLUT" "== 2.5.1.1"
    addRange "Haskell Platform" "hashable" "== 1.2.2.0"
    addRange "Haskell Platform" "haskell-src" "== 1.0.1.6"
    addRange "Haskell Platform" "html" "== 1.0.1.2"
    addRange "Haskell Platform" "HTTP" "== 4000.2.10"
    addRange "Haskell Platform" "HUnit" "== 1.2.5.2"
    addRange "Haskell Platform" "mtl" "== 2.1.3.1"
    addRange "Haskell Platform" "network" "== 2.4.2.3"
    addRange "Haskell Platform" "OpenGL" "== 2.9.2.0"
    addRange "Haskell Platform" "OpenGLRaw" "== 1.5.0.0"
    addRange "Haskell Platform" "parallel" "== 3.2.0.4"
    addRange "Haskell Platform" "parsec" "== 3.1.5"
    addRange "Haskell Platform" "primitive" "== 0.5.2.1"
    addRange "Haskell Platform" "QuickCheck" "== 2.6"
    addRange "Haskell Platform" "random" "== 1.0.1.1"
    addRange "Haskell Platform" "regex-base" "== 0.93.2"
    addRange "Haskell Platform" "regex-compat" "== 0.95.1"
    addRange "Haskell Platform" "regex-posix" "== 0.95.2"
    addRange "Haskell Platform" "split" "== 0.2.2"
    addRange "Haskell Platform" "stm" "== 2.4.2"
    addRange "Haskell Platform" "syb" "== 0.4.1"
    addRange "Haskell Platform" "text" "== 1.1.0.0"
    addRange "Haskell Platform" "transformers" "== 0.3.0.0"
    addRange "Haskell Platform" "unordered-containers" "== 0.2.4.0"
    addRange "Haskell Platform" "vector" "== 0.10.9.1"
    addRange "Haskell Platform" "xhtml" "== 3000.2.1"
    addRange "Haskell Platform" "zlib" "== 0.5.4.1"
    addRange "Haskell Platform" "alex" "== 3.1.3"
    addRange "Haskell Platform" "cabal-install" "== 1.18.0.5"
    addRange "Haskell Platform" "happy" "== 1.19.4"
    addRange "Haskell Platform" "hscolour" "== 1.20.3"

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
        , ("silkapp",      ["bergmark", "hesselink"])
        , ("snapframework",["mightybyte"])
        , ("haskell-ro",   ["mihaimaruseac"])
        ]
