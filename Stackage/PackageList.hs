module Stackage.PackageList where

import           Control.Monad        (foldM)
import           Data.Char            (isSpace)
import qualified Data.Map             as Map
import           Distribution.Text    (simpleParse)
import           Distribution.Version (anyVersion)
import           Stackage.Types

loadPackageList :: FilePath -> IO (Map PackageName VersionRange)
loadPackageList fp =
    readFile fp >>= foldM addLine Map.empty . lines
  where
    addLine ps l'
        | null l = return ps
        | otherwise =
            case parseVersionRange v' of
                Nothing -> error $ "Invalid version range: " ++ show (p, v')
                Just v -> return $ Map.insert (PackageName p) v ps
      where
        l = cleanup l'
        (p, v') = break isSpace l
    cleanup = dropWhile isSpace . reverse . dropWhile isSpace . reverse . stripComments

    parseVersionRange l
        | null $ cleanup l = Just anyVersion
        | otherwise = simpleParse l

    stripComments "" = ""
    stripComments ('-':'-':_) = ""
    stripComments (c:cs) = c : stripComments cs
