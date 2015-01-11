-- | Build everything with Shake.

module Stackage.ShakeBuild where

import           Stackage.BuildPlan
import           Stackage.PerformBuild (PerformBuild(..))

import qualified Data.Map.Strict as M
import           Data.Text (Text)
import           Development.Shake
import           Distribution.Text (display)
import           System.Directory
import           System.Environment

-- | Run the shake builder.
performBuild :: PerformBuild -> IO ()
performBuild pb = do
    shakeDir <- fmap (<//> "shake") getCurrentDirectory
    createDirectoryIfMissing True shakeDir
    withArgs
        []
        (shakeArgs
             shakeOptions {shakeFiles = shakeDir}
             (shakePlan pb shakeDir))

-- | The complete build plan as far as Shake is concerned.
shakePlan :: PerformBuild -> FilePath -> Rules ()
shakePlan pb shakeDir = do
    wantedFetched *> const (fetchedTarget wantedFetched pb)
    want [wantedFetched]
    where wantedFetched =
              shakeDir <//> "fetched"

-- | Make sure all package archives have been fetched.
fetchedTarget :: FilePath -> PerformBuild -> Action ()
fetchedTarget wantedFile pb =
    do () <- cmd
                 "cabal"
                 "fetch"
                 "--no-dependencies"
                 (map
                      (\(name,plan) ->
                            display name ++
                            "-" ++
                            display (ppVersion plan))
                      (M.toList
                           (bpPackages
                                (pbPlan pb))))
       liftIO (writeFile wantedFile "")
