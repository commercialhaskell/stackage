{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Stackage.GithubPings
    ( getGithubPings
    ) where

import Distribution.PackageDescription
import Stackage.BuildConstraints
import Stackage.Prelude

-- | Determine accounts to be pinged on Github based on various metadata in the
-- package description.
getGithubPings :: BuildConstraints -- ^ for mapping to pingees
               -> GenericPackageDescription -> Set Text
getGithubPings bc gpd =
    foldMap (\(pack -> name) -> fromMaybe (singletonSet name) (lookup name (bcGithubUsers bc))) $
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
