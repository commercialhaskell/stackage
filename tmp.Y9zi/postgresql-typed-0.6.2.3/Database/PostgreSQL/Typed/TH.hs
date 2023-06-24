{-# LANGUAGE CPP, PatternGuards, ScopedTypeVariables, FlexibleContexts, TemplateHaskell, DataKinds #-}
-- |
-- Module: Database.PostgreSQL.Typed.TH
-- Copyright: 2015 Dylan Simon
-- 
-- Support functions for compile-time PostgreSQL connection and state management.
-- You can use these to build your own Template Haskell functions using the PostgreSQL connection.

module Database.PostgreSQL.Typed.TH
  ( getTPGDatabase
  , withTPGTypeConnection
  , withTPGConnection
  , useTPGDatabase
  , reloadTPGTypes
  , TPGValueInfo(..)
  , tpgDescribe
  , tpgTypeEncoder
  , tpgTypeDecoder
  , tpgTypeBinary
  ) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative ((<$>), (<$))
#endif
import           Control.Applicative ((<|>))
import           Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar, withMVar)
import           Control.Exception (onException, finally)
#ifdef VERSION_tls
import           Control.Exception (throwIO)
#endif
import           Control.Monad (liftM2)
import qualified Data.ByteString as BS
#ifdef VERSION_tls
import qualified Data.ByteString.Char8 as BSC
#endif
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Foldable as Fold
import           Data.Maybe (isJust, fromMaybe)
import           Data.String (fromString)
import qualified Data.Traversable as Tv
import qualified Language.Haskell.TH as TH
import qualified Network.Socket as Net
import           System.Environment (lookupEnv)
import           System.IO.Unsafe (unsafePerformIO, unsafeInterleaveIO)

import Database.PostgreSQL.Typed.Types
import Database.PostgreSQL.Typed.Protocol
import Database.PostgreSQL.Typed.TypeCache

-- |Generate a 'PGDatabase' based on the environment variables:
-- @TPG_HOST@ (localhost); @TPG_SOCK@ or @TPG_PORT@ (5432); @TPG_DB@ or user; @TPG_USER@ or @USER@ (postgres); @TPG_PASS@ ()
getTPGDatabase :: IO PGDatabase
getTPGDatabase = do
  user <- fromMaybe "postgres" <$> liftM2 (<|>) (lookupEnv "TPG_USER") (lookupEnv "USER")
  db   <- fromMaybe user <$> lookupEnv "TPG_DB"
  host <- fromMaybe "localhost" <$> lookupEnv "TPG_HOST"
  pnum <- fromMaybe "5432" <$> lookupEnv "TPG_PORT"
#ifdef mingw32_HOST_OS
  let port = Right pnum
#else
  port <- maybe (Right pnum) Left <$> lookupEnv "TPG_SOCK"
#endif
  pass <- fromMaybe "" <$> lookupEnv "TPG_PASS"
  debug <- isJust <$> lookupEnv "TPG_DEBUG"
#ifdef VERSION_tls
  tlsEnabled <- isJust <$> lookupEnv "TPG_TLS"
  tlsVerifyMode <- lookupEnv "TPG_TLS_MODE" >>= \modeStr ->
    case modeStr of
      Just "full" -> pure TlsValidateFull
      Just "ca"   -> pure TlsValidateCA
      Just other  -> throwIO (userError ("Unknown verify mode: " ++ other))
      Nothing     -> pure TlsValidateCA
  mTlsCertPem <- lookupEnv "TPG_TLS_ROOT_CERT"
  dbTls <- case mTlsCertPem of
    Just certPem ->
      case pgTlsValidate tlsVerifyMode (BSC.pack certPem) of
        Right x  -> pure x
        Left err -> throwIO (userError err)
    Nothing | tlsEnabled -> pure TlsNoValidate
    Nothing -> pure TlsDisabled
#endif
  return $ defaultPGDatabase
    { pgDBAddr = either (Right . Net.SockAddrUnix) (Left . (,) host) port
    , pgDBName = BSU.fromString db
    , pgDBUser = BSU.fromString user
    , pgDBPass = BSU.fromString pass
    , pgDBDebug = debug
#ifdef VERSION_tls
    , pgDBTLS = dbTls
#endif
    }

{-# NOINLINE tpgState #-}
tpgState :: MVar (PGDatabase, Maybe PGTypeConnection)
tpgState = unsafePerformIO $ do
  db <- unsafeInterleaveIO getTPGDatabase
  newMVar (db, Nothing)

-- |Run an action using the Template Haskell state.
withTPGTypeConnection :: (PGTypeConnection -> IO a) -> IO a
withTPGTypeConnection f = do
  (db, tpg') <- takeMVar tpgState
  tpg <- maybe (newPGTypeConnection =<< pgConnect db) return tpg'
    `onException` putMVar tpgState (db, Nothing) -- might leave connection open
  f tpg `finally` putMVar tpgState (db, Just tpg)

-- |Run an action using the Template Haskell PostgreSQL connection.
withTPGConnection :: (PGConnection -> IO a) -> IO a
withTPGConnection f = withTPGTypeConnection (f . pgConnection)

-- |Specify an alternative database to use during compilation.
-- This lets you override the default connection parameters that are based on TPG environment variables.
-- This should be called as a top-level declaration and produces no code.
-- It uses 'pgReconnect' so is safe to call multiple times with the same database.
useTPGDatabase :: PGDatabase -> TH.DecsQ
useTPGDatabase db = TH.runIO $ do
  (db', tpg') <- takeMVar tpgState
  putMVar tpgState . (,) db =<<
    (if db == db'
      then Tv.mapM (\t -> do
        c <- pgReconnect (pgConnection t) db
        return t{ pgConnection = c }) tpg'
      else Nothing <$ Fold.mapM_ (pgDisconnect . pgConnection) tpg')
    `onException` putMVar tpgState (db, Nothing)
  return []

-- |Force reloading of all types from the database.
-- This may be needed if you make structural changes to the database during compile-time.
reloadTPGTypes :: TH.DecsQ
reloadTPGTypes = TH.runIO $ [] <$ withMVar tpgState (mapM_ flushPGTypeConnection . snd)

-- |Lookup a type name by OID.
-- Error if not found.
tpgType :: PGTypeConnection -> OID -> IO PGName
tpgType c o =
  maybe (fail $ "Unknown PostgreSQL type: " ++ show o ++ "\nYou may need to use reloadTPGTypes or adjust search_path, or your postgresql-typed application may need to be rebuilt.") return =<< lookupPGType c o

-- |Lookup a type OID by type name.
-- This is less common and thus less efficient than going the other way.
-- Fail if not found.
getTPGTypeOID :: PGTypeConnection -> PGName -> IO OID
getTPGTypeOID c t =
  maybe (fail $ "Unknown PostgreSQL type: " ++ show t ++ "; be sure to use the exact type name from \\dTS") return =<< findPGType c t

data TPGValueInfo = TPGValueInfo
  { tpgValueName :: BS.ByteString
  , tpgValueTypeOID :: !OID
  , tpgValueType :: PGName
  , tpgValueNullable :: Bool
  }

-- |A type-aware wrapper to 'pgDescribe'
tpgDescribe :: BS.ByteString -> [String] -> Bool -> IO ([TPGValueInfo], [TPGValueInfo])
tpgDescribe sql types nulls = withTPGTypeConnection $ \tpg -> do
  at <- mapM (getTPGTypeOID tpg . fromString) types
  (pt, rt) <- pgDescribe (pgConnection tpg) (BSL.fromStrict sql) at nulls
  (,)
    <$> mapM (\o -> do
      ot <- tpgType tpg o
      return TPGValueInfo
        { tpgValueName = BS.empty
        , tpgValueTypeOID = o
        , tpgValueType = ot
        , tpgValueNullable = True
        }) pt
    <*> mapM (\(c, o, n) -> do
      ot <- tpgType tpg o
      return TPGValueInfo
        { tpgValueName = c
        , tpgValueTypeOID = o
        , tpgValueType = ot
        , tpgValueNullable = n && o /= 2278 -- "void"
        }) rt

typeApply :: PGName -> TH.Name -> TH.Name -> TH.Exp
typeApply t f e =
  TH.VarE f `TH.AppE` TH.VarE e
    `TH.AppE` (TH.ConE 'PGTypeProxy `TH.SigE` (TH.ConT ''PGTypeID `TH.AppT` TH.LitT (TH.StrTyLit $ pgNameString $ t)))


-- |TH expression to encode a 'PGParameter' value to a 'Maybe' 'L.ByteString'.
tpgTypeEncoder :: Bool -> TPGValueInfo -> TH.Name -> TH.Exp
tpgTypeEncoder lit v = typeApply (tpgValueType v) $
  if lit
    then 'pgEscapeParameter
    else 'pgEncodeParameter

-- |TH expression to decode a 'Maybe' 'L.ByteString' to a ('Maybe') 'PGColumn' value.
tpgTypeDecoder :: Bool -> TPGValueInfo -> TH.Name -> TH.Exp
tpgTypeDecoder nulls v = typeApply (tpgValueType v) $
  if nulls && tpgValueNullable v
    then 'pgDecodeColumn
    else 'pgDecodeColumnNotNull

-- |TH expression calling 'pgBinaryColumn'.
tpgTypeBinary :: TPGValueInfo -> TH.Name -> TH.Exp
tpgTypeBinary v = typeApply (tpgValueType v) 'pgBinaryColumn
