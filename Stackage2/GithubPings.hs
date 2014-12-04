{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Stackage2.GithubPings
    ( getGithubPings
    ) where

import Stackage2.Prelude
import Distribution.PackageDescription
import qualified Stackage.Config as Old

-- | Determine accounts to be pinged on Github based on various metadata in the
-- package description.
getGithubPings :: GenericPackageDescription -> Set Text
getGithubPings gpd =
    setFromList $ map pack $ foldMap Old.convertGithubUser $
        goHomepage (homepage $ packageDescription gpd) ++
        concatMap goRepo (sourceRepos $ packageDescription gpd)
  where
    goHomepage t = do
        prefix <-
            [ "http://github.com/"
            , "https://github.com/"
            , "git://github.com/"
            , "git@github.com:"
            ]
        t' <- maybeToList $ stripPrefix prefix t
        let t'' = takeWhile (/= '/') t'
        guard $ not $ null t''
        return t''

    goRepo sr =
        case (repoType sr, repoLocation sr) of
            (Just Git, Just s) -> goHomepage s
            _ -> []
