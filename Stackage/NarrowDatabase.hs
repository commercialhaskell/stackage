module Stackage.NarrowDatabase where

import qualified Data.Map       as Map
import qualified Data.Set       as Set
import           Prelude        hiding (pi)
import           Stackage.Types

-- | Narrow down the database to only the specified packages and all of
-- their dependencies.
narrowPackageDB :: PackageDB
                -> Set PackageName
                -> IO (Map PackageName (Version, [PackageName]))
narrowPackageDB (PackageDB pdb) =
    loop Map.empty . Set.map ((,) [])
  where
    loop result toProcess =
        case Set.minView toProcess of
            Nothing -> return result
            Just ((users, p), toProcess') ->
                case Map.lookup p pdb of
                    Nothing
                        | null users -> error $ "Unknown package: " ++ show p
                        | otherwise -> loop result toProcess'
                    Just pi -> do
                        let users' = p:users
                            result' = Map.insert p (piVersion pi, users) result
                        loop result' $ Set.foldl' (addDep users' result') toProcess' $ piDeps pi
    addDep users result toProcess p =
        case Map.lookup p result of
            Nothing -> Set.insert (users, p) toProcess
            Just{} -> toProcess
