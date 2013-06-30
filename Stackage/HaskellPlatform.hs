module Stackage.HaskellPlatform
    ( loadHaskellPlatform
    ) where

import           Control.Monad     (guard)
import           Data.Char         (isSpace)
import           Data.List         (foldl', isInfixOf, isPrefixOf, stripPrefix)
import           Data.Maybe        (mapMaybe)
import           Data.Monoid       (Monoid (..))
import           Data.Set          (singleton)
import           Distribution.Text (simpleParse)
import           Stackage.Types
import System.Directory (doesFileExist)
import System.FilePath ((</>))

loadHaskellPlatform :: SelectSettings -> IO (Maybe HaskellPlatform)
loadHaskellPlatform ss = do
    e <- doesFileExist fp
    if e
        then fmap (Just . parseHP) $ readFile fp
        else do
            putStrLn "Warning: No Haskell Platform found for current GHC version"
            return Nothing
  where
    GhcMajorVersion x y = selectGhcVersion ss

    fp = haskellPlatformDir ss </> (concat
        [ "haskell-platform-"
        , show x
        , "."
        , show y
        , ".cabal"
        ])

data HPLine = HPLPackage PackageIdentifier
            | HPLBeginCore
            | HPLEndCore
            | HPLBeginPlatform
            | HPLEndPlatform
    deriving Show

toHPLine :: String -> Maybe HPLine
toHPLine s
    | "begin core packages" `isInfixOf` s = Just HPLBeginCore
    | "end core packages" `isInfixOf` s = Just HPLEndCore
    | "begin platform packages" `isInfixOf` s = Just HPLBeginPlatform
    | "end platform packages" `isInfixOf` s = Just HPLEndPlatform
    | otherwise = do
        let s1 = dropWhile isSpace s
        guard $ not $ "--" `isPrefixOf` s1
        guard $ not $ null s1
        guard $ "==" `isInfixOf` s1
        let (package', s2) = break (== '=') s1
            package = takeWhile (not . isSpace) package'
        s3 <- stripPrefix "==" s2
        version <- simpleParse $ takeWhile (/= ',') s3
        Just $ HPLPackage $ PackageIdentifier (PackageName package) version

parseHP :: String -> HaskellPlatform
parseHP =
    snd . foldl' addLine (notInBlock, mempty) . mapMaybe toHPLine . lines
  where
    notInBlock _ = mempty
    inCore x = HaskellPlatform (singleton x) mempty
    inPlatform x = HaskellPlatform mempty (singleton x)

    addLine (fromPackage, hp) (HPLPackage vp) = (fromPackage, fromPackage vp `mappend` hp)
    addLine (_, hp) HPLBeginCore = (inCore, hp)
    addLine (_, hp) HPLEndCore = (notInBlock, hp)
    addLine (_, hp) HPLBeginPlatform = (inPlatform, hp)
    addLine (_, hp) HPLEndPlatform = (notInBlock, hp)
