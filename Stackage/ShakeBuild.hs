-- | Build everything with Shake.

module Stackage.ShakeBuild where

import           Stackage.BuildPlan
import           Stackage.PackageDescription
import           Stackage.BuildConstraints
import           Stackage.PerformBuild (PerformBuild(..))

import           Control.Monad
import           Data.List ((\\))
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import           Development.Shake
import           Distribution.Package (PackageName)
import           Distribution.Text (display)
import           System.Directory
import           System.Environment

-- | Run the shake builder.
performBuild :: PerformBuild -> IO ()
performBuild pb = do
    shakeDir <- fmap (<//> "shake/") getCurrentDirectory
    withArgs
        []
        (shakeArgs
             shakeOptions {shakeFiles = shakeDir}
             (shakePlan pb shakeDir))

-- | The complete build plan as far as Shake is concerned.
shakePlan :: PerformBuild -> FilePath -> Rules ()
shakePlan pb shakeDir = do
    fetched <- target (targetForFetched shakeDir) $
               fetchedTarget shakeDir pb
    _ <- forM corePackages $
         \name ->
              let fp =
                      targetForPackage shakeDir name
              in target fp (makeFile fp)
    packageTargets <- forM normalPackages $
                      \(name,plan) ->
                           target
                               (targetForPackage shakeDir name)
                               (do need [fetched]
                                   packageTarget shakeDir name plan)
    want packageTargets
    where corePackages =
              M.keys $ siCorePackages $ bpSystemInfo $ pbPlan pb
          normalPackages =
              filter (not . (`elem` corePackages) . fst) $
              M.toList $ bpPackages $ pbPlan pb

-- | Make sure all package archives have been fetched.
fetchedTarget :: FilePath -> PerformBuild -> Action ()
fetchedTarget shakeDir pb = do
    () <- cmd "cabal" "fetch" "--no-dependencies" $
          map
              (\(name,plan) ->
                    display name ++
                    "-" ++
                    display (ppVersion plan))
              (M.toList
                   (bpPackages
                        (pbPlan pb)))
    makeFile (targetForFetched shakeDir)

-- | Build, test and generate documentation for the package.
packageTarget :: FilePath -> PackageName -> PackagePlan -> Action ()
packageTarget shakeDir name plan = do
    need (map (targetForPackage shakeDir)
              (M.keys (sdPackages (ppDesc plan))))
    () <- cmd (Cwd shakeDir) "cabal" "unpack" nameVer
    () <- cmd (Cwd pkgDir) "cabal" "configure"
    () <- cmd (Cwd pkgDir) "cabal" "build"
    () <- cmd (Cwd pkgDir) "cabal" "copy"
    () <- cmd (Cwd pkgDir) "cabal" "register"
    makeFile (targetForPackage shakeDir name)
    where pkgDir =
              shakeDir <//> nameVer
          nameVer =
              display name ++
              "-" ++
              display (ppVersion plan)

-- | Get the target file for confirming that all packages have been
-- pre-fetched.
targetForFetched :: FilePattern -> FilePattern
targetForFetched shakeDir =
    shakeDir <//> "fetched"

-- | Get the target file for a package.
targetForPackage :: FilePattern -> PackageName -> FilePattern
targetForPackage shakeDir name =
    shakeDir <//> "packages" <//> display name

-- | Declare a target, returning the target name.
target :: FilePattern -> Action () -> Rules FilePattern
target name action = do
    name *> const action
    return name

-- | Make a file of this name.
makeFile :: FilePath -> Action ()
makeFile fp = liftIO $ writeFile fp ""
