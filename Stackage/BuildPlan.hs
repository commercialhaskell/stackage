{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
module Stackage.BuildPlan
    ( readBuildPlan
    , writeBuildPlan
    ) where

import Stackage.Types
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Distribution.Text       (simpleParse, display)
import           Distribution.Package (PackageName (..))
import Control.Applicative ((<$>), (<*>))

readBuildPlan :: FilePath -> IO BuildPlan
readBuildPlan fp = do
    str <- readFile fp
    case fromString str of
        Left s -> error $ "Could not read build plan: " ++ s
        Right (x, "") -> return x
        Right (_, _:_) -> error "Trailing content when reading build plan"

writeBuildPlan :: FilePath -> BuildPlan -> IO ()
writeBuildPlan fp bp = writeFile fp $ toString bp

class AsString a where
    toString :: a -> String
    fromString :: String -> Either String (a, String)

instance AsString BuildPlan where
    toString BuildPlan {..} = concat
        [ makeSection "tools" bpTools
        , makeSection "packages" $ Map.toList bpPackages
        , makeSection "core" $ Set.toList bpCore
        , makeSection "optional-core" $ Map.toList bpOptionalCore
        ]
    fromString s1 = do
        (tools, s2) <- getSection "tools" s1
        (packages, s3) <- getSection "packages" s2
        (core, s4) <- getSection "core" s3
        (optionalCore, s5) <- getSection "optional-core" s4
        let bp = BuildPlan
                { bpTools = tools
                , bpPackages = Map.fromList packages
                , bpCore = Set.fromList core
                , bpOptionalCore = Map.fromList optionalCore
                }
        return (bp, s5)

makeSection :: AsString a => String -> [a] -> String
makeSection title contents = unlines
    $ ("-- BEGIN " ++ title)
    : map toString contents
   ++ ["-- END " ++ title, ""]

instance AsString String where
    toString = id
    fromString s = Right (s, "")

instance AsString PackageName where
    toString (PackageName pn) = pn
    fromString s = Right (PackageName s, "")

instance AsString a => AsString (PackageName, a) where
    toString (PackageName pn, s) = concat [pn, " ", toString s]
    fromString s = do
        (pn, rest) <- takeWord s
        (rest', s') <- fromString rest
        return ((PackageName pn, rest'), s')

takeWord :: AsString a => String -> Either String (a, String)
takeWord s =
    case break (== ' ') s of
        (x, _:y) -> do
            (x', s') <- fromString x
            if null s'
                then Right (x', y)
                else Left $ "Unconsumed input in takeWord call"

instance AsString SelectedPackageInfo where
    toString SelectedPackageInfo {..} = unwords
        [ display spiVersion
        , toString spiHasTests
        , maybe "@" ("@" ++) spiGithubUser
        , unMaintainer spiMaintainer
        ]
    fromString s1 = do
        (version, s2) <- takeWord s1
        (hasTests, s3) <- takeWord s2
        (gu, m) <- takeWord s3
        Right (SelectedPackageInfo
            { spiVersion = version
            , spiHasTests = hasTests
            , spiGithubUser = gu
            , spiMaintainer = Maintainer m
            }, "")

instance AsString (Maybe String) where
    toString Nothing = "@"
    toString (Just x) = "@" ++ x
    fromString "@" = Right (Nothing, "")
    fromString ('@':rest) = Right (Just rest, "")
    fromString x = Left $ "Invalid Github user: " ++ x

instance AsString Bool where
    toString True = "test"
    toString False = "notest"
    fromString "test" = Right (True, "")
    fromString "notest" = Right (False, "")
    fromString x = Left $ "Invalid test value: " ++ x

instance AsString Version where
    toString = display
    fromString s =
        case simpleParse s of
            Nothing -> Left $ "Invalid version: " ++ s
            Just v -> Right (v, "")

getSection :: AsString a => String -> String -> Either String ([a], String)
getSection title orig =
    case lines orig of
        [] -> Left "Unexpected EOF when looking for a section"
        l1:ls1
            | l1 == begin ->
                case break (== end) ls1 of
                    (here, _:"":rest) -> do
                        here' <- mapM fromString' here
                        Right (here', unlines rest)
                    (_, _) -> Left $ "Could not find section end: " ++ title
            | otherwise -> Left $ "Could not find section start: " ++ title
  where
    begin = "-- BEGIN " ++ title
    end = "-- END " ++ title

    fromString' x = do
        (y, z) <- fromString x
        if null z
            then return y
            else Left $ "Unconsumed input on line: " ++ x
