module Stackage.Init (stackageInit) where

import           Data.List       (isInfixOf, isPrefixOf)
import           Stackage.Util
import           System.FilePath ((</>))

stackageInit :: IO ()
stackageInit = do
    c <- getCabalRoot
    let config = c </> "config"
    orig <- readFile config
    -- bypass laziness
    _ <- return $! length orig
    writeFile config $ unlines $ go $ lines orig
  where
    go = addStackage
       . map commentHackage
       . filter (\s -> not $ "stackage" `isInfixOf` s)

    addStackage [] = stackageLines []
    addStackage (l:ls)
        | "remote-repo-cache:" `isPrefixOf` l = stackageLines $ l : ls
        | otherwise = l : addStackage ls

    stackageLines x =
        "remote-repo: stackage:http://hackage.haskell.org/packages/archive"
      : "remote-repo: stackage-extra:http://hackage.haskell.org/packages/archive"
      : x

    commentHackage s
        | s == "remote-repo: hackage.haskell.org:http://hackage.haskell.org/packages/archive" = "--" ++ s
        | otherwise = s
