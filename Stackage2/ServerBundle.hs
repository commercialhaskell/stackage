-- | Create a bundle to be uploaded to Stackage Server.
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Stackage2.ServerBundle
    ( serverBundle
    , epochTime
    , bpAllPackages
    ) where

import qualified Codec.Archive.Tar          as Tar
import qualified Codec.Archive.Tar.Entry    as Tar
import qualified Codec.Compression.GZip     as GZip
import qualified Data.Yaml                  as Y
import           Foreign.C.Types            (CTime (CTime))
import           Stackage2.BuildConstraints
import           Stackage2.BuildPlan
import           Stackage2.Prelude
import qualified System.PosixCompat.Time    as PC

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
