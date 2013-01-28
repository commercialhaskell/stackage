module Stackage.CheckCabalVersion
    ( checkCabalVersion
    ) where

import           Control.Exception    (assert)
import           Distribution.Text    (simpleParse)
import           Distribution.Version (withinRange)
import           Prelude              hiding (pi)
import           System.Process       (readProcess)

checkCabalVersion :: IO String
checkCabalVersion = do
    putStrLn "Checking Cabal version"
    versionString <- readProcess "cabal" ["--version"] ""
    libVersion <-
        case map words $ lines versionString of
            [_,["using","version",libVersion,"of","the","Cabal","library"]] -> return libVersion
            _ -> error "Did not understand cabal --version output"

    case (simpleParse libVersion, simpleParse ">= 1.16") of
        (Nothing, _) -> error $ "cabal binary reported an invalid Cabal library version: " ++ libVersion
        (_, Nothing) -> assert False $ return ()
        (Just v, Just vr)
            | v `withinRange` vr -> return ()
            | otherwise -> error $ "cabal binary build against unsupported Cabal library version: " ++ libVersion

    return libVersion
