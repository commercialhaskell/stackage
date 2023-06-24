{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds #-}
module Main (main) where

import qualified Data.ByteString as BS
import           Data.Int (Int16, Int32, Int64)
import qualified Data.Time as Time
import qualified Criterion.Main as C
import           System.Exit (exitSuccess, exitFailure)

import Database.PostgreSQL.Typed
import Database.PostgreSQL.Typed.Types
import Database.PostgreSQL.Typed.Query

import Connect

useTPGDatabase db

selectTypes :: PGConnection -> IO [(String, OID, Int16, Bool, Maybe BS.ByteString)]
selectTypes c = pgQuery c [pgSQL|SELECT typname, typnamespace, typlen, typbyval, typdefault FROM pg_catalog.pg_type|]

selectTypesLazy :: PGConnection -> IO [(String, OID, Int16, Bool, Maybe BS.ByteString)]
selectTypesLazy c = pgLazyQuery c [pgSQL|$SELECT typname, typnamespace, typlen, typbyval, typdefault FROM pg_catalog.pg_type|] 1

selectParams :: PGConnection -> IO [(Maybe String, Maybe Int64, Maybe Double, Maybe BS.ByteString, Maybe Bool)]
selectParams c = pgQuery c [pgSQL|$SELECT ${"hello"}::text, ${123::Int64}::bigint, ${123.4::Double}::float, ${BS.pack [120..220]}::bytea, ${Nothing::Maybe Bool}::boolean|]

selectValues :: PGConnection -> IO [(Int32, Time.UTCTime)]
selectValues c = pgQuery c [pgSQL|!SELECT generate_series, now() FROM generate_series(8,256)|]

selectValuesLazy :: PGConnection -> IO [(Int32, Time.UTCTime)]
selectValuesLazy c = pgLazyQuery c [pgSQL|$!SELECT generate_series, now() FROM generate_series(8,256)|] 5

main :: IO ()
main = do
  c <- pgConnect db
  C.defaultMain
    [ C.bench "types" $ C.nfIO $ selectTypes c
    , C.bench "types lazy" $ C.nfIO $ selectTypesLazy c
    , C.bench "params" $ C.nfIO $ selectParams c
    , C.bench "values" $ C.nfIO $ selectValues c
    , C.bench "values lazy" $ C.nfIO $ selectValuesLazy c
    ]
