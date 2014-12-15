{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
module Stackage.CorePackagesSpec (spec) where

import Stackage.CorePackages
import Stackage.Prelude
import Test.Hspec

spec :: Spec
spec = do
    it "works" $ void getCorePackages
    it "contains known core packages" $ do
        m <- getCorePackages
        forM_ (words "ghc containers base") $ \p ->
            m `shouldSatisfy` (member (PackageName p))
    it "getCoreExecutables includes known executables" $ do
        s <- getCoreExecutables
        s `shouldSatisfy` member "ghc"
        s `shouldSatisfy` member "hsc2hs"
        s `shouldSatisfy` member "runghc"
