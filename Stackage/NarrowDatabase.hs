module Stackage.NarrowDatabase where

import qualified Data.Map       as Map
import qualified Data.Set       as Set
import           Prelude        hiding (pi)
import           Stackage.Types

-- | Narrow down the database to only the specified packages and all of
-- their dependencies.
narrowPackageDB :: PackageDB
                -> Set PackageName
                -> IO (Map PackageName Version)
narrowPackageDB (PackageDB pdb) =
    loop Map.empty . Set.map ((,) True)
  where
    loop result toProcess =
        case Set.minView toProcess of
            Nothing -> return result
            Just ((isOrig, p), toProcess') ->
                case Map.lookup p pdb of
                    Nothing
                        | isOrig -> error $ "Unknown package: " ++ show p
                        | otherwise -> loop result toProcess'
                    Just pi -> do
                        let result' = Map.insert p (piVersion pi) result
                        loop result' $ Set.foldl' (addDep result') toProcess' $ piDeps pi
    addDep result toProcess p =
        case Map.lookup p result of
            Nothing -> Set.insert (False, p) toProcess
            Just{} -> toProcess
