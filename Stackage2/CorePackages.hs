{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Stackage2.CorePackages
    ( getCorePackages
    , getCoreExecutables
    ) where

import qualified Data.Text         as T
import           Filesystem        (listDirectory)
import           Stackage2.Prelude
import           System.Directory  (findExecutable)

-- | Get a @Map@ of all of the core packages. Core packages are defined as
-- packages which ship with GHC itself.
--
-- Precondition: GHC global package database has only core packages, and GHC
-- ships with just a single version of each packages.
getCorePackages :: IO (Map PackageName Version)
getCorePackages =
    withCheckedProcess cp $ \ClosedStream src Inherited ->
        src $$ decodeUtf8C =$ linesUnboundedC =$ foldMapMC parsePackage
  where
    cp = proc "ghc-pkg" ["--no-user-package-conf", "list"]
    parsePackage t
        | ":" `isInfixOf` t = return mempty
        | Just p <- stripSuffix "-" p' = singletonMap
            <$> simpleParse p
            <*> simpleParse v
        | otherwise = return mempty
      where
        (p', v) = T.breakOnEnd "-" $ dropParens $ T.strip t

        dropParens s
            | length s > 2 && headEx s == '(' && lastEx s == ')' =
                initEx $ tailEx s
            | otherwise = s

-- | A list of executables that are shipped with GHC.
getCoreExecutables :: IO (Set ExeName)
getCoreExecutables = do
    mfp <- findExecutable "ghc"
    dir <-
        case mfp of
            Nothing -> error "No ghc executable found on PATH"
            Just fp -> return $ directory $ fpFromString fp
    (setFromList . map (ExeName . fpToText . filename)) <$> listDirectory dir
