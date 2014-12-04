{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Stackage2.CorePackagesSpec (spec) where

import Stackage2.CorePackages
import Stackage2.Prelude
import Test.Hspec

spec :: Spec
spec = do
    it "works" $ void getCorePackages
    it "contains known core packages" $ do
        m <- getCorePackages
        forM_ (words "ghc containers base") $ \p ->
            m `shouldSatisfy` (member (PackageName p))
