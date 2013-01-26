module Stackage.CheckCabalVersion
    ( checkCabalVersion
    ) where

import           Control.Exception    (assert)
import           Control.Monad        (unless, when)
import qualified Data.Map             as Map
import           Data.Maybe           (mapMaybe)
import           Data.Set             (empty)
import qualified Data.Set             as Set
import           Distribution.Text    (simpleParse)
import           Distribution.Version (withinRange)
import           Prelude              hiding (pi)
import           Stackage.CheckPlan
import           Stackage.Config
import           Stackage.InstallInfo
import           Stackage.Tarballs
import           Stackage.Test
import           Stackage.Types
import           Stackage.Util
import           System.Directory     (canonicalizePath,
                                       createDirectoryIfMissing,
                                       doesDirectoryExist)
import           System.Exit          (ExitCode (ExitSuccess), exitWith)
import           System.IO            (IOMode (WriteMode), hPutStrLn,
                                       withBinaryFile)
import           System.Process       (rawSystem, readProcess, runProcess,
                                       waitForProcess)

checkCabalVersion :: IO String
checkCabalVersion = do
    versionString <- readProcess "cabal" ["--version"] ""
    libVersion <-
        case map words $ lines versionString of
            [_,["using","version",libVersion,"of","the","Cabal","library"]] -> return libVersion
            _ -> error "Did not understand cabal --version output"

    case (simpleParse libVersion, simpleParse ">= 1.16") of
        (Nothing, _) -> error $ "Invalid Cabal library version: " ++ libVersion
        (_, Nothing) -> assert False $ return ()
        (Just v, Just vr)
            | v `withinRange` vr -> return ()
            | otherwise -> error $ "Unsupported Cabal library version: " ++ libVersion

    return libVersion
