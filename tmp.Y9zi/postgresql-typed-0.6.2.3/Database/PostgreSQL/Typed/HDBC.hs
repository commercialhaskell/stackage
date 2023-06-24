-- |
-- Module: Database.PostgreSQL.Typed.HDBC
-- Copyright: 2016 Dylan Simon
-- 
-- Use postgresql-typed as a backend for HDBC.
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Database.PostgreSQL.Typed.HDBC
  ( Connection
  , connect
  , fromPGConnection
  , withPGConnection
  , reloadTypes
  , connectionFetchSize
  , setFetchSize
  ) where

import Control.Arrow ((&&&))
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (handle, throwIO)
import Control.Monad (void, guard)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import Data.Int (Int16)
import qualified Data.IntMap.Lazy as IntMap
import Data.List (uncons)
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Time.Clock (DiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (zonedTimeToUTC)
import Data.Word (Word32)
import qualified Database.HDBC.Types as HDBC
import qualified Database.HDBC.ColTypes as HDBC
import System.Mem.Weak (addFinalizer)
import Text.Read (readMaybe)

import Database.PostgreSQL.Typed.Types
import Database.PostgreSQL.Typed.Protocol
import Database.PostgreSQL.Typed.Dynamic
import Database.PostgreSQL.Typed.TypeCache
import Database.PostgreSQL.Typed.SQLToken
import Paths_postgresql_typed (version)

-- |A wrapped 'PGConnection'.
-- This differs from a bare 'PGConnection' in a few ways:
--
--   1. It always has exactly one active transaction (with 'pgBegin')
--   2. It automatically disconnects on GC
--   3. It provides a mutex around the underlying 'PGConnection' for thread-safety
--
data Connection = Connection
  { connectionPG :: MVar PGConnection
  , connectionServerVer :: String
  , connectionTypes :: IntMap.IntMap SqlType
  , connectionFetchSize :: Word32 -- ^Number of rows to fetch (and cache) with 'HDBC.execute' and each time 'HDBC.fetchRow' requires more rows. A higher value will result in fewer round-trips to the database but potentially more wasted data. Defaults to 1. 0 means fetch all rows.
  }

sqlError :: IO a -> IO a
sqlError = handle $ \(PGError m) -> 
  let f c = BSC.unpack $ Map.findWithDefault BSC.empty c m
      fC = f 'C'
      fD = f 'D' in
  throwIO HDBC.SqlError 
    { HDBC.seState = fC
    , HDBC.seNativeError = if null fC then -1 else fromMaybe 0 $ readMaybe (f 'P')
    , HDBC.seErrorMsg = f 'S' ++ ": " ++ f 'M' ++ if null fD then fD else '\n':fD
    }

-- |Use the underlying 'PGConnection' directly. You must be careful to ensure that the first invariant is preserved: you should not call 'pgBegin', 'pgCommit', or 'pgRollback' on it. All other operations should be safe.
withPGConnection :: Connection -> (PGConnection -> IO a) -> IO a
withPGConnection c = sqlError . withMVar (connectionPG c)

takePGConnection :: PGConnection -> IO (MVar PGConnection)
takePGConnection pg = do
  addFinalizer pg (pgDisconnectOnce pg)
  pgBegin pg
  newMVar pg

-- |Convert an existing 'PGConnection' to an HDBC-compatible 'Connection'.
-- The caveats under 'connectionPG' apply if you plan to continue using the original 'PGConnection'.
fromPGConnection :: PGConnection -> IO Connection
fromPGConnection pg = do
  pgv <- takePGConnection pg
  reloadTypes Connection
    { connectionPG = pgv
    , connectionServerVer = maybe "" BSC.unpack $ pgServerVersion $ pgTypeEnv pg
    , connectionTypes = mempty
    , connectionFetchSize = 1
    }

-- |Connect to a database for HDBC use (equivalent to 'pgConnect' and 'pgBegin').
connect :: PGDatabase -> IO Connection
connect d = sqlError $ do
  pg <- pgConnect d
  fromPGConnection pg

-- |Reload the table of all types from the database.
-- This may be needed if you make structural changes to the database.
reloadTypes :: Connection -> IO Connection
reloadTypes c = withPGConnection c $ \pg -> do
  t <- pgGetTypes pg
  return c{ connectionTypes = IntMap.map (sqlType (pgTypeEnv pg) . pgNameString) t }

-- |Change the 'connectionFetchSize' for new 'HDBC.Statement's created with 'HDBC.prepare'.
-- Ideally this could be set with each call to 'HDBC.execute' and 'HDBC.fetchRow', but the HDBC interface provides no way to do this.
setFetchSize :: Word32 -> Connection -> Connection
setFetchSize i c = c{ connectionFetchSize = i }

sqls :: String -> BSLC.ByteString
sqls = BSLC.pack

placeholders :: Int -> [SQLToken] -> [SQLToken]
placeholders n (SQLQMark False : l) = SQLParam n : placeholders (succ n) l
placeholders n (SQLQMark True : l) = SQLQMark False : placeholders n l
placeholders n (t : l) = t : placeholders n l
placeholders _ [] = []

data ColDesc = ColDesc
  { colDescName :: String
  , colDesc :: HDBC.SqlColDesc
  , colDescDecode :: PGValue -> HDBC.SqlValue
  }

data Cursor = Cursor
  { cursorDesc :: [ColDesc]
  , cursorRow :: [PGValues]
  , cursorActive :: Bool
  , _cursorStatement :: HDBC.Statement -- keep a handle to prevent GC
  }

noCursor :: HDBC.Statement -> Cursor
noCursor = Cursor [] [] False

getType :: Connection -> PGConnection -> Maybe Bool -> PGColDescription -> ColDesc
getType c pg nul PGColDescription{..} = ColDesc
  { colDescName = BSC.unpack pgColName
  , colDesc = HDBC.SqlColDesc
    { HDBC.colType = sqlTypeId t
    , HDBC.colSize = fromIntegral pgColModifier <$ guard (pgColModifier >= 0)
    , HDBC.colOctetLength = fromIntegral pgColSize <$ guard (pgColSize >= 0)
    , HDBC.colDecDigits = Nothing
    , HDBC.colNullable = nul
    }
  , colDescDecode = sqlTypeDecode t
  } where t = IntMap.findWithDefault (sqlType (pgTypeEnv pg) $ show pgColType) (fromIntegral pgColType) (connectionTypes c)

instance HDBC.IConnection Connection where
  disconnect c = withPGConnection c
    pgDisconnectOnce
  commit c = withPGConnection c $ \pg -> do
    pgCommitAll pg
    pgBegin pg
  rollback c = withPGConnection c $ \pg -> do
    pgRollbackAll pg
    pgBegin pg
  runRaw c q = withPGConnection c $ \pg ->
    pgSimpleQueries_ pg $ sqls q
  run c q v = withPGConnection c $ \pg -> do
    let q' = sqls $ show $ placeholders 1 $ sqlTokens q
        v' = map encode v
    fromMaybe 0 <$> pgRun pg q' [] v'
  prepare c q = do
    let q' = sqls $ show $ placeholders 1 $ sqlTokens q
    n <- withPGConnection c $ \pg -> pgPrepare pg q' []
    cr <- newIORef $ error "Cursor"
    let
      execute v = withPGConnection c $ \pg -> do
        d <- pgBind pg n (map encode v)
        (r, e) <- pgFetch pg n (connectionFetchSize c)
        modifyIORef' cr $ \p -> p
          { cursorDesc = map (getType c pg Nothing) d
          , cursorRow = r
          , cursorActive = isNothing e
          }
        return $ fromMaybe 0 e
      stmt = HDBC.Statement
        { HDBC.execute = execute
        , HDBC.executeRaw = void $ execute []
        , HDBC.executeMany = mapM_ execute
        , HDBC.finish = withPGConnection c $ \pg -> do
          writeIORef cr $ noCursor stmt
          pgClose pg n
        , HDBC.fetchRow = withPGConnection c $ \pg -> do
          p <- readIORef cr
          fmap (zipWith colDescDecode (cursorDesc p)) <$> case cursorRow p of
            [] | cursorActive p -> do
                (rl, e) <- pgFetch pg n (connectionFetchSize c)
                let rl' = uncons rl
                writeIORef cr p
                  { cursorRow = maybe [] snd rl'
                  , cursorActive = isNothing e
                  }
                return $ fst <$> rl'
               | otherwise ->
                return Nothing
            (r:l) -> do
              writeIORef cr p{ cursorRow = l }
              return $ Just r
        , HDBC.getColumnNames =
          map colDescName . cursorDesc <$> readIORef cr
        , HDBC.originalQuery = q
        , HDBC.describeResult =
          map (colDescName &&& colDesc) . cursorDesc <$> readIORef cr
        }
    writeIORef cr $ noCursor stmt
    addFinalizer stmt $ withPGConnection c $ \pg -> pgClose pg n
    return stmt
  clone c = withPGConnection c $ \pg -> do
    pg' <- pgConnect $ pgConnectionDatabase pg
    pgv <- takePGConnection pg'
    return c{ connectionPG = pgv }
  hdbcDriverName _ = "postgresql-typed"
  hdbcClientVer _ = show version
  proxiedClientName = HDBC.hdbcDriverName
  proxiedClientVer = HDBC.hdbcClientVer
  dbServerVer = connectionServerVer
  dbTransactionSupport _ = True
  getTables c = withPGConnection c $ \pg ->
    map (pgDecodeRep . head) . snd <$> pgSimpleQuery pg (BSLC.fromChunks
      [ "SELECT relname"
      ,  " FROM pg_catalog.pg_class"
      ,  " JOIN pg_catalog.pg_namespace ON relnamespace = pg_namespace.oid"
      , " WHERE nspname = ANY (current_schemas(false))"
      ,   " AND relkind IN ('r','v','m','f')"
      ])
  describeTable c t = withPGConnection c $ \pg ->
    map (\[attname, attrelid, attnum, atttypid, attlen, atttypmod, attnotnull] ->
      colDescName &&& colDesc $ getType c pg (Just $ not $ pgDecodeRep attnotnull) PGColDescription
        { pgColName = pgDecodeRep attname
        , pgColTable = pgDecodeRep attrelid
        , pgColNumber = pgDecodeRep attnum
        , pgColType = pgDecodeRep atttypid
        , pgColSize = pgDecodeRep attlen
        , pgColModifier = pgDecodeRep atttypmod
        , pgColBinary = False
        })
      . snd <$> pgSimpleQuery pg (BSLC.fromChunks
        [ "SELECT attname, attrelid, attnum, atttypid, attlen, atttypmod, attnotnull"
        ,  " FROM pg_catalog.pg_attribute"
        , " WHERE attrelid = ", pgLiteralRep t, "::regclass"
        , "   AND attnum > 0 AND NOT attisdropped"
        , " ORDER BY attrelid, attnum"
        ])

encodeRep :: PGRep a => a -> PGValue
encodeRep x = PGTextValue $ pgEncode (pgTypeOf x) x

encode :: HDBC.SqlValue -> PGValue
encode (HDBC.SqlString x)                 = encodeRep x
encode (HDBC.SqlByteString x)             = encodeRep x
encode (HDBC.SqlWord32 x)                 = encodeRep x
encode (HDBC.SqlWord64 x)                 = encodeRep (fromIntegral x :: Rational)
encode (HDBC.SqlInt32 x)                  = encodeRep x
encode (HDBC.SqlInt64 x)                  = encodeRep x
encode (HDBC.SqlInteger x)                = encodeRep (fromInteger x :: Rational)
encode (HDBC.SqlChar x)                   = encodeRep x
encode (HDBC.SqlBool x)                   = encodeRep x
encode (HDBC.SqlDouble x)                 = encodeRep x
encode (HDBC.SqlRational x)               = encodeRep x
encode (HDBC.SqlLocalDate x)              = encodeRep x
encode (HDBC.SqlLocalTimeOfDay x)         = encodeRep x
encode (HDBC.SqlZonedLocalTimeOfDay t z)  = encodeRep (t, z)
encode (HDBC.SqlLocalTime x)              = encodeRep x
encode (HDBC.SqlZonedTime x)              = encodeRep (zonedTimeToUTC x)
encode (HDBC.SqlUTCTime x)                = encodeRep x
encode (HDBC.SqlDiffTime x)               = encodeRep (realToFrac x :: DiffTime)
encode (HDBC.SqlPOSIXTime x)              = encodeRep (realToFrac x :: Rational) -- (posixSecondsToUTCTime x)
encode (HDBC.SqlEpochTime x)              = encodeRep (posixSecondsToUTCTime (fromInteger x))
encode (HDBC.SqlTimeDiff x)               = encodeRep (fromIntegral x :: DiffTime)
encode HDBC.SqlNull = PGNullValue

data SqlType = SqlType
  { sqlTypeId :: HDBC.SqlTypeId
  , sqlTypeDecode :: PGValue -> HDBC.SqlValue
  }

sqlType :: PGTypeEnv -> String -> SqlType
sqlType e t = SqlType
  { sqlTypeId = typeId t
  , sqlTypeDecode = decode t e
  }

typeId :: String -> HDBC.SqlTypeId
typeId "boolean"                      = HDBC.SqlBitT
typeId "bytea"                        = HDBC.SqlVarBinaryT
typeId "\"char\""                     = HDBC.SqlCharT
typeId "name"                         = HDBC.SqlVarCharT
typeId "bigint"                       = HDBC.SqlBigIntT
typeId "smallint"                     = HDBC.SqlSmallIntT
typeId "integer"                      = HDBC.SqlIntegerT
typeId "text"                         = HDBC.SqlLongVarCharT
typeId "oid"                          = HDBC.SqlIntegerT
typeId "real"                         = HDBC.SqlFloatT
typeId "double precision"             = HDBC.SqlDoubleT
typeId "abstime"                      = HDBC.SqlUTCDateTimeT
typeId "reltime"                      = HDBC.SqlIntervalT HDBC.SqlIntervalSecondT
typeId "tinterval"                    = HDBC.SqlIntervalT HDBC.SqlIntervalDayToSecondT
typeId "bpchar"                       = HDBC.SqlVarCharT
typeId "character varying"            = HDBC.SqlVarCharT
typeId "date"                         = HDBC.SqlDateT
typeId "time without time zone"       = HDBC.SqlTimeT
typeId "timestamp without time zone"  = HDBC.SqlTimestampT
typeId "timestamp with time zone"     = HDBC.SqlTimestampWithZoneT -- XXX really SQLUTCDateTimeT
typeId "interval"                     = HDBC.SqlIntervalT HDBC.SqlIntervalDayToSecondT
typeId "time with time zone"          = HDBC.SqlTimeWithZoneT
typeId "numeric"                      = HDBC.SqlDecimalT
typeId "uuid"                         = HDBC.SqlGUIDT
typeId t = HDBC.SqlUnknownT t

decodeRep :: PGColumn t a => PGTypeID t -> PGTypeEnv -> (a -> HDBC.SqlValue) -> PGValue -> HDBC.SqlValue
decodeRep t e f (PGBinaryValue v) = f $ pgDecodeBinary e t v
decodeRep t _ f (PGTextValue v) = f $ pgDecode t v
decodeRep _ _ _ PGNullValue = HDBC.SqlNull

#define DECODE(T) \
  decode T e = decodeRep (PGTypeProxy :: PGTypeID T) e

decode :: String -> PGTypeEnv -> PGValue -> HDBC.SqlValue
DECODE("boolean")                     HDBC.SqlBool
DECODE("\"char\"")                    HDBC.SqlChar
DECODE("name")                        HDBC.SqlString
DECODE("bigint")                      HDBC.SqlInt64
DECODE("smallint")                    (HDBC.SqlInt32 . fromIntegral :: Int16 -> HDBC.SqlValue)
DECODE("integer")                     HDBC.SqlInt32
DECODE("text")                        HDBC.SqlString
DECODE("oid")                         HDBC.SqlWord32
DECODE("real")                        HDBC.SqlDouble
DECODE("double precision")            HDBC.SqlDouble
DECODE("bpchar")                      HDBC.SqlString
DECODE("character varying")           HDBC.SqlString
DECODE("date")                        HDBC.SqlLocalDate
DECODE("time without time zone")      HDBC.SqlLocalTimeOfDay
DECODE("time with time zone")         (uncurry HDBC.SqlZonedLocalTimeOfDay)
DECODE("timestamp without time zone") HDBC.SqlLocalTime
DECODE("timestamp with time zone")    HDBC.SqlUTCTime
DECODE("interval")                    (HDBC.SqlDiffTime . realToFrac :: DiffTime -> HDBC.SqlValue)
DECODE("numeric")                     HDBC.SqlRational
decode _ _ = decodeRaw where
  decodeRaw (PGBinaryValue v) = HDBC.SqlByteString v
  decodeRaw (PGTextValue v)   = HDBC.SqlByteString v
  decodeRaw PGNullValue       = HDBC.SqlNull
