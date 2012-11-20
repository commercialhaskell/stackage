import qualified Data.Map                 as Map
import qualified Data.Set                 as Set
import           Data.Version             (showVersion)
import           Stackage.HaskellPlatform
import           Stackage.LoadDatabase
import           Stackage.NarrowDatabase
import           Stackage.PackageList
import           Stackage.Types
import           Stackage.Util

main :: IO ()
main = do
    userPackages <- loadPackageList "package-list.txt"
    hp <- loadHaskellPlatform
    let allPackages = Map.union userPackages $ identsToRanges (hplibs hp)
    pdb <- loadPackageDB (Set.map (\(PackageIdentifier p _) -> p) (hpcore hp)) allPackages
    final <- narrowPackageDB pdb $ Set.fromList $ Map.keys allPackages
    putStr "cabal-dev install -fnetwork23 --enable-tests "
    mapM_ (\(PackageName p, v) -> putStr $ p ++ "-" ++ showVersion v ++ " ") $ Map.toList final
