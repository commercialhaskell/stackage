{-# LANGUAGE OverloadedStrings #-}
module Database.PostgreSQL.Typed.TypeCache
  ( PGTypes
  , pgGetTypes
  , PGTypeConnection
  , pgConnection
  , newPGTypeConnection
  , flushPGTypeConnection
  , lookupPGType
  , findPGType
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.IntMap as IntMap
import Data.List (find)

import Database.PostgreSQL.Typed.Types (PGName, OID)
import Database.PostgreSQL.Typed.Dynamic
import Database.PostgreSQL.Typed.Protocol

-- |Map keyed on fromIntegral OID.
type PGTypes = IntMap.IntMap PGName

-- |A 'PGConnection' along with cached information about types.
data PGTypeConnection = PGTypeConnection
  { pgConnection :: !PGConnection
  , pgTypes :: IORef (Maybe PGTypes)
  }

-- |Create a 'PGTypeConnection'.
newPGTypeConnection :: PGConnection -> IO PGTypeConnection
newPGTypeConnection c = do
  t <- newIORef Nothing
  return $ PGTypeConnection c t

-- |Flush the cached type list, forcing it to be reloaded.
flushPGTypeConnection :: PGTypeConnection -> IO ()
flushPGTypeConnection c =
  writeIORef (pgTypes c) Nothing

-- |Get a map of types from the database.
pgGetTypes :: PGConnection -> IO PGTypes
pgGetTypes c =
  IntMap.fromAscList . map (\[to, tn] -> (fromIntegral (pgDecodeRep to :: OID), pgDecodeRep tn)) .
    snd <$> pgSimpleQuery c "SELECT oid, format_type(CASE WHEN typtype = 'd' THEN typbasetype ELSE oid END, -1) FROM pg_catalog.pg_type ORDER BY oid"

-- |Get a cached map of types.
getPGTypes :: PGTypeConnection -> IO PGTypes
getPGTypes (PGTypeConnection c tr) =
  maybe (do
      t <- pgGetTypes c
      writeIORef tr $ Just t
      return t)
    return
    =<< readIORef tr

-- |Lookup a type name by OID.
-- This is an efficient, often pure operation.
lookupPGType :: PGTypeConnection -> OID -> IO (Maybe PGName)
lookupPGType c o =
  IntMap.lookup (fromIntegral o) <$> getPGTypes c

-- |Lookup a type OID by type name.
-- This is less common and thus less efficient than going the other way.
findPGType :: PGTypeConnection -> PGName -> IO (Maybe OID)
findPGType c t =
  fmap (fromIntegral . fst) . find ((==) t . snd) . IntMap.toList <$> getPGTypes c
