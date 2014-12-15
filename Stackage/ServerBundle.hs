-- | Create a bundle to be uploaded to Stackage Server.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Stackage.ServerBundle
    ( serverBundle
    , epochTime
    , bpAllPackages
    , docsListing
    ) where

import qualified Codec.Archive.Tar          as Tar
import qualified Codec.Archive.Tar.Entry    as Tar
import qualified Codec.Compression.GZip     as GZip
import qualified Data.Yaml                  as Y
import           Foreign.C.Types            (CTime (CTime))
import           Stackage.BuildConstraints
import           Stackage.BuildPlan
import           Stackage.Prelude
import qualified System.PosixCompat.Time    as PC
import qualified Text.XML as X
import Text.XML.Cursor
import Filesystem (isFile)

-- | Get current time
epochTime :: IO Tar.EpochTime
epochTime = (\(CTime t) -> t) <$> PC.epochTime

-- | All package/versions in a build plan, including core packages.
--
-- Note that this may include packages not available on Hackage.
bpAllPackages :: BuildPlan -> Map PackageName Version
bpAllPackages BuildPlan {..} =
    siCorePackages bpSystemInfo ++ map ppVersion bpPackages

serverBundle :: Tar.EpochTime
             -> Text -- ^ title
             -> Text -- ^ slug
             -> BuildPlan
             -> LByteString
serverBundle time title slug bp@BuildPlan {..} = GZip.compress $ Tar.write
    [ fe "build-plan.yaml" (fromStrict $ Y.encode bp)
    , fe "hackage" hackage
    , fe "slug" (fromStrict $ encodeUtf8 slug)
    , fe "desc" (fromStrict $ encodeUtf8 title)
    ]
  where
    fe name contents =
        case Tar.toTarPath False name of
            Left s -> error s
            Right name' -> (Tar.fileEntry name' contents)
                { Tar.entryTime = time
                }
    hackage = builderToLazy $ foldMap goPair $ mapToList packageMap

    -- need to remove some packages that don't exist on Hackage
    packageMap = foldr deleteMap (bpAllPackages bp) $ map PackageName
        [ "bin-package-db"
        , "ghc"
        , "rts"
        ]

    goPair (name, version) =
        toBuilder (display name) ++
        toBuilder (asText "-") ++
        toBuilder (display version) ++
        toBuilder (asText "\n")

docsListing :: BuildPlan
            -> FilePath -- ^ docs directory
            -> IO ByteString
docsListing bp docsDir =
    fmap (Y.encode . fold) $ mapM go $ mapToList $ bpAllPackages bp
  where
    go :: (PackageName, Version) -> IO (Map Text Y.Value)
    go (package, version) = do -- handleAny (const $ return mempty) $ do
        let dirname = fpFromText (concat
                [ display package
                , "-"
                , display version
                ])
            indexFP = (docsDir </> dirname </> "index.html")
        ie <- isFile indexFP
        if ie
            then do
                doc <- flip X.readFile indexFP X.def
                    { X.psDecodeEntities = X.decodeHtmlEntities
                    }
                let cursor = fromDocument doc
                    getPair x = take 1 $ do
                        href <- attribute "href" x
                        let name = concat $ x $// content
                        guard $ not $ null name
                        return (href, name)
                    pairs = cursor $// attributeIs "class" "module"
                                   &/ laxElement "a" >=> getPair
                m <- fmap fold $ forM pairs $ \(href, name) -> do
                    let suffix = dirname </> fpFromText href
                    e <- isFile $ docsDir </> suffix
                    return $ if e
                        then asMap $ singletonMap name [fpToText dirname, href]
                        else mempty
                return $ singletonMap (display package) $ Y.object
                    [ "version" Y..= display version
                    , "modules" Y..= m
                    ]
            else return mempty
