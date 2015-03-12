-- | General commands related to ghc-pkg.

module Stackage.GhcPkg where

import           Control.Monad
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Conduit.Process
import qualified Data.Conduit.Text as CT
import           Data.List
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Distribution.Compat.ReadP
import           Distribution.Package
import           Distribution.Text (display)
import           Distribution.Text (parse)
import           Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as FP
import           Prelude hiding (FilePath)

-- | Get broken packages.
getBrokenPackages :: FilePath -> IO [PackageIdentifier]
getBrokenPackages dir = do
    (_,ps) <- sourceProcessWithConsumer
                  (proc
                       "ghc-pkg"
                       ["check", "--simple-output", "-f", FP.encodeString dir])
                  (CT.decodeUtf8 $= CT.lines $= CL.consume)
    return (mapMaybe parsePackageIdent (T.words (T.unlines ps)))

-- | Get available packages.
getRegisteredPackages :: FilePath -> IO [PackageIdentifier]
getRegisteredPackages dir = do
    (_,ps) <- sourceProcessWithConsumer
                  (proc
                       "ghc-pkg"
                       ["list", "--simple-output", "-f", FP.encodeString dir])
                  (CT.decodeUtf8 $= CT.lines $= CL.consume)
    return (mapMaybe parsePackageIdent (T.words (T.unlines ps)))

-- | Parse a package identifier: foo-1.2.3
parsePackageIdent :: Text -> Maybe PackageIdentifier
parsePackageIdent = fmap fst .
    listToMaybe .
    filter (null . snd) .
    readP_to_S parse . T.unpack

-- | Unregister a package.
unregisterPackage :: FilePath -> PackageName -> IO ()
unregisterPackage dir ident = do
    void (readProcessWithExitCode
              "ghc-pkg"
              ["unregister", "-f", FP.encodeString dir, "--force", display ident]
              "")
