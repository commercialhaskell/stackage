-- | Review the upload log and compare against a locally kept list of allowed
-- uploaders.
module Stackage.Uploads
    ( checkUploads
    , printForbidden
    , filterForbidden
    ) where

import           Control.Exception (assert, evaluate)
import           Control.Monad     (forM_, unless)
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Maybe        (fromMaybe)
import           Data.Monoid       (Monoid (..))
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           Data.Time         (UTCTime, parseTime)
import           Network.HTTP      (getRequest, getResponseBody, simpleHTTP)
import qualified Stackage.Types    as ST
import           System.Directory  (doesFileExist)
import           System.Exit       (exitFailure)
import           System.Locale

checkUploads :: FilePath -- ^ allowed
             -> FilePath -- ^ new allowed
             -> IO Forbidden
checkUploads allowedFP newAllowedFP = do
    putStrLn "Getting upload log"
    uploadLog <- getUploadLog

    putStrLn "Reading allowed uploaders list"
    allowed <- readAllowed allowedFP

    putStrLn "Computing new and forbidden uploaders"
    let (allowed', forbidden) = updateAllowed uploadLog allowed
    _ <- evaluate $ msize allowed'
    _ <- evaluate $ msize forbidden

    unless (Map.null $ unMonoidMap allowed') $ do
        putStrLn $ "Newly uploaded packages detected, writing to " ++ newAllowedFP
        writeAllowed newAllowedFP allowed'

    return forbidden

-- Define a Map newtype wrapper with a proper Monoid instance.
newtype MonoidMap k v = MonoidMap { unMonoidMap :: Map k v }
instance (Ord k, Monoid v) => Monoid (MonoidMap k v) where
    mempty = MonoidMap mempty
    MonoidMap x `mappend` MonoidMap y = MonoidMap $ Map.unionWith mappend x y

-- And some helper functions.
mlookup :: Ord k => k -> MonoidMap k v -> Maybe v
mlookup k = Map.lookup k . unMonoidMap

msingleton :: k -> v -> MonoidMap k v
msingleton k = MonoidMap . Map.singleton k

mkeys :: MonoidMap k v -> [k]
mkeys = Map.keys . unMonoidMap

msize :: MonoidMap k v -> Int
msize = Map.size . unMonoidMap

type Version = String
type UserName = String
type PackageName = String

type PackageHistory = Map Version (UserName, UTCTime)
type UploadLog = MonoidMap PackageName PackageHistory
type Allowed = MonoidMap PackageName (Set UserName)
type Forbidden = UploadLog

getUploadLog :: IO UploadLog
getUploadLog = do
    rsp <- simpleHTTP $ getRequest logURL
    body <- getResponseBody rsp
    return $ mconcat $ map go $ lines body
  where
    go :: String -> UploadLog
    go s = fromMaybe mempty $ do
        ver:pkg:user:date' <- Just $ reverse $ words s
        t <- parseTime defaultTimeLocale fmtStr $ unwords $ reverse date'
        Just $ msingleton pkg $ Map.singleton ver (user, t)

    logURL :: String
    logURL = "http://hackage.haskell.org/packages/archive/log"

    fmtStr :: String
    fmtStr = "%a %b %e %T %Z %Y"

readAllowed :: FilePath -> IO Allowed
readAllowed fp = do
    exists <- doesFileExist fp
    if exists
        then do
            s <- readFile fp
            return $ mconcat $ map go $ lines s
        else return mempty
  where
    go :: String -> Allowed
    go s = fromMaybe mempty $ do
        pkg:users <- Just $ words s
        Just $ msingleton pkg $ Set.fromList users

updateAllowed :: UploadLog -> Allowed -> (Allowed, Forbidden)
updateAllowed uploads allowed =
    mconcat $ map go $ Set.toList allPackages
  where
    -- Map.keys uploads should be sufficient, but being redundant to ensure we
    -- never lose any data from allowed.
    allPackages = Set.fromList (mkeys uploads) `Set.union`
                  Set.fromList (mkeys allowed)

    go :: PackageName -> (Allowed, Forbidden)
    go pkg =
        case (mlookup pkg uploads, mlookup pkg allowed) of
            (Nothing, Nothing) -> assert False (mempty, mempty)
            (Nothing, Just _) -> (mempty, mempty)
            (Just u, Nothing) -> (msingleton pkg $ getAllUsers u, mempty)
            (Just u, Just a) ->
                let fval = mconcat $ map (check a) $ Map.toList u
                    forbidden
                        | Map.null fval = mempty
                        | otherwise = msingleton pkg fval
                 in (mempty, forbidden)

    getAllUsers :: PackageHistory -> Set UserName
    getAllUsers = Set.fromList . map fst . Map.elems

    check :: Set UserName -> (Version, (UserName, UTCTime)) -> Map Version (UserName, UTCTime)
    check allowed' (ver, (user, time))
        | user `Set.member` allowed' = Map.empty
        | otherwise = Map.singleton ver (user, time)

writeAllowed :: FilePath -> Allowed -> IO ()
writeAllowed fp =
    writeFile fp . unlines . map go . Map.toList . unMonoidMap
  where
    go (pkg, users) = unwords $ pkg : Set.toList users

printForbidden :: Forbidden -> IO ()
printForbidden (MonoidMap forbidden) = unless (Map.null forbidden) $ do
    putStrLn $ "Following uploads were forbidden:"
    forM_ (Map.toList forbidden) $ \(pkg, cases) -> do
        putStrLn ""
        putStrLn pkg
        forM_ (Map.toList cases) $ \(version, (user, time)) ->
            putStrLn $ concat
                [ "Version "
                , version
                , " by "
                , user
                , " at "
                , show time
                ]
    exitFailure

filterForbidden :: ST.BuildPlan
                -> Forbidden
                -> Forbidden
filterForbidden bp =
    MonoidMap . Map.filterWithKey isIncluded . unMonoidMap
  where
    isIncluded pn _ = ST.PackageName pn `Set.member` allPackages
    allPackages =
        Map.keysSet (ST.bpPackages bp) `Set.union`
        ST.bpCore bp `Set.union`
        Map.keysSet (ST.bpOptionalCore bp)
