module Stackage.NarrowDatabase where

import           Control.Monad.Trans.Writer
import qualified Data.Map       as Map
import qualified Data.Set       as Set
import           Prelude        hiding (pi)
import           Stackage.Types
import           Stackage.Util
import           System.Exit (exitFailure)

-- | Narrow down the database to only the specified packages and all of
-- their dependencies.
narrowPackageDB :: BuildSettings
                -> PackageDB
                -> Set (PackageName, Maintainer)
                -> IO (Map PackageName BuildInfo)
narrowPackageDB settings (PackageDB pdb) packageSet = do
    (res, errs) <- runWriterT $ loop Map.empty $ Set.map (\(name, maintainer) -> ([], name, maintainer)) packageSet
    if Set.null errs
        then return res
        else do
            putStrLn "Build plan requires some disallowed packages"
            mapM_ putStrLn $ Set.toList errs
            exitFailure
  where
    loop result toProcess =
        case Set.minView toProcess of
            Nothing -> return result
            Just ((users, p, maintainer), toProcess') ->
                case Map.lookup p pdb of
                    Nothing
                        | null users -> error $ "Unknown package: " ++ show p
                        | otherwise -> loop result toProcess'
                    Just pi -> do
                        let users' = p:users
                            result' = Map.insert p BuildInfo
                                { biVersion    = piVersion pi
                                , biUsers      = users
                                , biMaintainer = maintainer
                                , biDeps       = piDeps pi
                                } result
                        case piGPD pi of
                            Nothing -> return ()
                            Just gpd ->
                                case allowedPackage settings gpd of
                                    Left msg -> tell $ Set.singleton $ concat
                                        [ packageVersionString (p, piVersion pi)
                                        , ": "
                                        , msg
                                        ]
                                    Right () -> return ()
                        loop result' $ Set.foldl' (addDep users' result' maintainer) toProcess' $ Map.keysSet $ piDeps pi
    addDep users result maintainer toProcess p =
        case Map.lookup p result of
            Nothing -> Set.insert (users, p, maintainer) toProcess
            Just{} -> toProcess
