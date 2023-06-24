{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
-- Copyright 2010, 2011, 2012, 2013 Chris Forno
-- Copyright 2014-2018 Dylan Simon

-- |The Protocol module allows for direct, low-level communication with a
--  PostgreSQL server over TCP/IP. You probably don't want to use this module
--  directly.

module Database.PostgreSQL.Typed.Protocol ( 
    PGDatabase(..)
  , defaultPGDatabase
  , PGConnection
  , PGError(..)
#ifdef VERSION_tls
  , PGTlsMode(..)
  , PGTlsValidateMode (..)
#endif
  , pgErrorCode
  , pgConnectionDatabase
  , pgTypeEnv
  , pgConnect
  , pgDisconnect
  , pgReconnect
  -- * Query operations
  , pgDescribe
  , pgSimpleQuery
  , pgSimpleQueries_
  , pgPreparedQuery
  , pgPreparedLazyQuery
  , pgCloseStatement
  -- * Transactions
  , pgBegin
  , pgCommit
  , pgRollback
  , pgCommitAll
  , pgRollbackAll
  , pgTransaction
  -- * HDBC support
  , pgDisconnectOnce
  , pgRun
  , PGPreparedStatement
  , pgPrepare
  , pgClose
  , PGColDescription(..)
  , PGRowDescription
  , pgBind
  , pgFetch
  -- * Notifications
  , PGNotification(..)
  , pgGetNotification
  , pgGetNotifications
#ifdef VERSION_tls
  -- * TLS Helpers
  , pgTlsValidate
#endif
  , pgSupportsTls
  ) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative ((<$>), (<$))
#endif
import           Control.Arrow ((&&&), first, second)
import           Control.Exception (Exception, onException, finally, throwIO)
#ifdef VERSION_tls
import           Control.Exception (catch)
#endif
import           Control.Monad (void, liftM2, replicateM, when, unless)
#ifdef VERSION_cryptonite
import qualified Crypto.Hash as Hash
import qualified Data.ByteArray.Encoding as BA
#endif
import qualified Data.Binary.Get as G
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Internal (w2c, createAndTrim)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.ByteString.Lazy.Internal (smallChunkSize)
#ifdef VERSION_tls
import           Data.Default (def)
#endif
import qualified Data.Foldable as Fold
import           Data.IORef (IORef, newIORef, writeIORef, readIORef, atomicModifyIORef, atomicModifyIORef', modifyIORef')
import           Data.Int (Int32, Int16)
import qualified Data.Map.Lazy as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
#if !MIN_VERSION_base(4,8,0)
import           Data.Monoid (mempty)
#endif
import           Data.Time.Clock (getCurrentTime)
import           Data.Tuple (swap)
import           Data.Typeable (Typeable)
#if !MIN_VERSION_base(4,8,0)
import           Data.Word (Word)
#endif
import           Data.Word (Word32, Word8)
#ifdef VERSION_tls
import           Data.X509 (SignedCertificate, HashALG(HashSHA256))
import           Data.X509.Memory (readSignedObjectFromMemory)
import           Data.X509.CertificateStore (makeCertificateStore)
import qualified Data.X509.Validation
#endif
#ifndef mingw32_HOST_OS
import           Foreign.C.Error (eWOULDBLOCK, getErrno, errnoToIOError)
import           Foreign.C.Types (CChar(..), CInt(..), CSize(..))
import           Foreign.Ptr (Ptr, castPtr)
import           GHC.IO.Exception (IOErrorType(InvalidArgument))
#endif
import qualified Network.Socket as Net
import qualified Network.Socket.ByteString as NetBS
import qualified Network.Socket.ByteString.Lazy as NetBSL
#ifdef VERSION_tls
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS
#endif
import           System.IO (stderr, hPutStrLn)
import           System.IO.Error (IOError, mkIOError, eofErrorType, ioError, ioeSetErrorString)
import           System.IO.Unsafe (unsafeInterleaveIO)
import           Text.Read (readMaybe)
import           Text.Show.Functions ()

import Database.PostgreSQL.Typed.Types
import Database.PostgreSQL.Typed.Dynamic

data PGState
  = StateUnsync -- no Sync
  | StatePending -- expecting ReadyForQuery
  -- ReadyForQuery received:
  | StateIdle
  | StateTransaction
  | StateTransactionFailed
  -- Terminate sent or EOF received
  | StateClosed
  deriving (Show, Eq)

#ifdef VERSION_tls
data PGTlsValidateMode
  = TlsValidateFull
  -- ^ Equivalent to sslmode=verify-full. Ie: Check the FQHN against the
  -- certicate's CN
  | TlsValidateCA
  -- ^ Equivalent to sslmode=verify-ca. Ie: Only check that the certificate has
  -- been signed by the root certificate we provide
  deriving (Show, Eq)

data PGTlsMode
  = TlsDisabled
  -- ^ TLS is disabled
  | TlsNoValidate
  | TlsValidate PGTlsValidateMode SignedCertificate
  deriving (Eq, Show)

-- | Constructs a 'PGTlsMode' to validate the server certificate with given root
-- certificate (in PEM format)
pgTlsValidate :: PGTlsValidateMode -> BSC.ByteString -> Either String PGTlsMode
pgTlsValidate mode certPem =
  case readSignedObjectFromMemory certPem of
    []  -> Left "Could not parse any certificate in PEM"
    (x:_) -> Right (TlsValidate mode x)

pgSupportsTls :: PGConnection -> Bool
pgSupportsTls PGConnection{connHandle=PGTlsContext _} = True
pgSupportsTls _ = False
#else
pgSupportsTls :: PGConnection -> Bool
pgSupportsTls _ = False
#endif

-- |Information for how to connect to a database, to be passed to 'pgConnect'.
data PGDatabase = PGDatabase
  { pgDBAddr :: Either (Net.HostName, Net.ServiceName) Net.SockAddr -- ^ The address to connect to the server
  , pgDBName :: BS.ByteString -- ^ The name of the database
  , pgDBUser, pgDBPass :: BS.ByteString
  , pgDBParams :: [(BS.ByteString, BS.ByteString)] -- ^ Extra parameters to set for the connection (e.g., ("TimeZone", "UTC"))
  , pgDBDebug :: Bool -- ^ Log all low-level server messages
  , pgDBLogMessage :: MessageFields -> IO () -- ^ How to log server notice messages (e.g., @print . PGError@)
#ifdef VERSION_tls
  , pgDBTLS :: PGTlsMode -- ^ TLS mode
#endif
  } deriving (Show)

instance Eq PGDatabase where
#ifdef VERSION_tls
  PGDatabase a1 n1 u1 p1 l1 _ _ s1 == PGDatabase a2 n2 u2 p2 l2 _ _ s2 =
    a1 == a2 && n1 == n2 && u1 == u2 && p1 == p2 && l1 == l2 && s1 == s2
#else
  PGDatabase a1 n1 u1 p1 l1 _ _ == PGDatabase a2 n2 u2 p2 l2 _ _ =
    a1 == a2 && n1 == n2 && u1 == u2 && p1 == p2 && l1 == l2
#endif

newtype PGPreparedStatement = PGPreparedStatement Integer
  deriving (Eq, Show)

preparedStatementName :: PGPreparedStatement -> BS.ByteString
preparedStatementName (PGPreparedStatement n) = BSC.pack $ show n

data PGHandle
  = PGSocket Net.Socket
#ifdef VERSION_tls
  | PGTlsContext TLS.Context
#endif

pgPutBuilder :: PGHandle -> B.Builder -> IO ()
pgPutBuilder (PGSocket s) b = NetBSL.sendAll s (B.toLazyByteString b)
#ifdef VERSION_tls
pgPutBuilder (PGTlsContext c) b = TLS.sendData c (B.toLazyByteString b)
#endif

pgPut:: PGHandle -> BS.ByteString -> IO ()
pgPut (PGSocket s) bs = NetBS.sendAll s bs
#ifdef VERSION_tls
pgPut (PGTlsContext c) bs = TLS.sendData c (BSL.fromChunks [bs])
#endif

pgGetSome :: PGHandle -> Int -> IO BSC.ByteString
pgGetSome (PGSocket s) count = NetBS.recv s count
#ifdef VERSION_tls
pgGetSome (PGTlsContext c) _ = TLS.recvData c
#endif

pgCloseHandle :: PGHandle -> IO ()
pgCloseHandle (PGSocket s) = Net.close s
#ifdef VERSION_tls
pgCloseHandle (PGTlsContext c) = do
  TLS.bye c `catch` \(_ :: IOError) -> pure ()
  TLS.contextClose c
#endif

pgFlush :: PGConnection -> IO ()
pgFlush PGConnection{connHandle=PGSocket _} = pure ()
#ifdef VERSION_tls
pgFlush PGConnection{connHandle=PGTlsContext c} = TLS.contextFlush c
#endif

-- |An established connection to the PostgreSQL server.
-- These objects are not thread-safe and must only be used for a single request at a time.
data PGConnection = PGConnection
  { connHandle :: PGHandle
  , connDatabase :: !PGDatabase
  , connPid :: !Word32 -- unused
  , connKey :: !Word32 -- unused
  , connTypeEnv :: PGTypeEnv
  , connParameters :: IORef (Map.Map BS.ByteString BS.ByteString)
  , connPreparedStatementCount :: IORef Integer
  , connPreparedStatementMap :: IORef (Map.Map (BS.ByteString, [OID]) PGPreparedStatement)
  , connState :: IORef PGState
  , connInput :: IORef (G.Decoder PGBackendMessage)
  , connTransaction :: IORef Word
  , connNotifications :: IORef (Queue PGNotification)
  }

data PGColDescription = PGColDescription
  { pgColName :: BS.ByteString
  , pgColTable :: !OID
  , pgColNumber :: !Int16
  , pgColType :: !OID
  , pgColSize :: !Int16
  , pgColModifier :: !Int32
  , pgColBinary :: !Bool
  } deriving (Show)
type PGRowDescription = [PGColDescription]

type MessageFields = Map.Map Char BS.ByteString

data PGNotification = PGNotification
  { pgNotificationPid :: !Word32
  , pgNotificationChannel :: !BS.ByteString
  , pgNotificationPayload :: BSL.ByteString
  } deriving (Show)

-- |Simple amortized fifo
data Queue a = Queue [a] [a]

emptyQueue :: Queue a
emptyQueue = Queue [] []

enQueue :: a -> Queue a -> Queue a
enQueue a (Queue e d) = Queue (a:e) d

deQueue :: Queue a -> (Queue a, Maybe a)
deQueue (Queue e (x:d)) = (Queue e d, Just x)
deQueue (Queue (reverse -> x:d) []) = (Queue [] d, Just x)
deQueue q = (q, Nothing)

-- |PGFrontendMessage represents a PostgreSQL protocol message that we'll send.
-- See <http://www.postgresql.org/docs/current/interactive/protocol-message-formats.html>.
data PGFrontendMessage
  = StartupMessage [(BS.ByteString, BS.ByteString)] -- only sent first
  | CancelRequest !Word32 !Word32 -- sent first on separate connection
  | Bind { portalName :: BS.ByteString, statementName :: BS.ByteString, bindParameters :: PGValues, binaryColumns :: [Bool] }
  | CloseStatement { statementName :: BS.ByteString }
  | ClosePortal { portalName :: BS.ByteString }
  -- |Describe a SQL query/statement. The SQL string can contain
  --  parameters ($1, $2, etc.).
  | DescribeStatement { statementName :: BS.ByteString }
  | DescribePortal { portalName :: BS.ByteString }
  | Execute { portalName :: BS.ByteString, executeRows :: !Word32 }
  | Flush
  -- |Parse SQL Destination (prepared statement)
  | Parse { statementName :: BS.ByteString, queryString :: BSL.ByteString, parseTypes :: [OID] }
  | PasswordMessage BS.ByteString
  -- |SimpleQuery takes a simple SQL string. Parameters ($1, $2,
  --  etc.) aren't allowed.
  | SimpleQuery { queryString :: BSL.ByteString }
  | Sync
  | Terminate
  deriving (Show)

-- |PGBackendMessage represents a PostgreSQL protocol message that we'll receive.
-- See <http://www.postgresql.org/docs/current/interactive/protocol-message-formats.html>.
data PGBackendMessage
  = AuthenticationOk
  | AuthenticationCleartextPassword
  | AuthenticationMD5Password BS.ByteString
  -- AuthenticationSCMCredential
  | BackendKeyData Word32 Word32
  | BindComplete
  | CloseComplete
  | CommandComplete BS.ByteString
  -- |Each DataRow (result of a query) is a list of 'PGValue', which are assumed to be text unless known to be otherwise.
  | DataRow PGValues
  | EmptyQueryResponse
  -- |An ErrorResponse contains the severity, "SQLSTATE", and
  --  message of an error. See
  --  <http://www.postgresql.org/docs/current/interactive/protocol-error-fields.html>.
  | ErrorResponse { messageFields :: MessageFields }
  | NoData
  | NoticeResponse { messageFields :: MessageFields }
  | NotificationResponse PGNotification
  -- |A ParameterDescription describes the type of a given SQL
  --  query/statement parameter ($1, $2, etc.). Unfortunately,
  --  PostgreSQL does not give us nullability information for the
  --  parameter.
  | ParameterDescription [OID]
  | ParameterStatus BS.ByteString BS.ByteString
  | ParseComplete
  | PortalSuspended
  | ReadyForQuery PGState
  -- |A RowDescription contains the name, type, table OID, and
  --  column number of the resulting columns(s) of a query. The
  --  column number is useful for inferring nullability.
  | RowDescription PGRowDescription
  deriving (Show)

-- |PGException is thrown upon encountering an 'ErrorResponse' with severity of
--  ERROR, FATAL, or PANIC. It holds the message of the error.
newtype PGError = PGError { pgErrorFields :: MessageFields }
  deriving (Typeable)

instance Show PGError where
  show (PGError m) = displayMessage m

instance Exception PGError

-- |Produce a human-readable string representing the message
displayMessage :: MessageFields -> String
displayMessage m = "PG" ++ f 'S' ++ (if null fC then ": " else " [" ++ fC ++ "]: ") ++ f 'M' ++ (if null fD then fD else '\n' : fD)
  where
  fC = f 'C'
  fD = f 'D'
  f c = BSC.unpack $ Map.findWithDefault BS.empty c m

makeMessage :: BS.ByteString -> BS.ByteString -> MessageFields
makeMessage m d = Map.fromAscList [('D', d), ('M', m)]

-- |Message SQLState code.
--  See <http://www.postgresql.org/docs/current/static/errcodes-appendix.html>.
pgErrorCode :: PGError -> BS.ByteString
pgErrorCode (PGError e) = Map.findWithDefault BS.empty 'C' e

defaultLogMessage :: MessageFields -> IO ()
defaultLogMessage = hPutStrLn stderr . displayMessage

-- |A database connection with sane defaults:
-- localhost:5432:postgres
defaultPGDatabase :: PGDatabase
defaultPGDatabase = PGDatabase
  { pgDBAddr = Right $ Net.SockAddrInet 5432 (Net.tupleToHostAddress (127,0,0,1))
  , pgDBName = "postgres"
  , pgDBUser = "postgres"
  , pgDBPass = BS.empty
  , pgDBParams = []
  , pgDBDebug = False
  , pgDBLogMessage = defaultLogMessage
#ifdef VERSION_tls
  , pgDBTLS = TlsDisabled
#endif
  }

connDebugMsg :: PGConnection -> String -> IO ()
connDebugMsg c msg = when (pgDBDebug $ connDatabase c) $ do
  t <- getCurrentTime
  hPutStrLn stderr $ show t ++ msg

connLogMessage :: PGConnection -> MessageFields -> IO ()
connLogMessage = pgDBLogMessage . connDatabase

-- |The database information for this connection.
pgConnectionDatabase :: PGConnection -> PGDatabase
pgConnectionDatabase = connDatabase

-- |The type environment for this connection.
pgTypeEnv :: PGConnection -> PGTypeEnv
pgTypeEnv = connTypeEnv

#ifdef VERSION_cryptonite
md5 :: BS.ByteString -> BS.ByteString
md5 = BA.convertToBase BA.Base16 . (Hash.hash :: BS.ByteString -> Hash.Digest Hash.MD5)
#endif


nul :: B.Builder
nul = B.word8 0

byteStringNul :: BS.ByteString -> B.Builder
byteStringNul s = B.byteString s <> nul

lazyByteStringNul :: BSL.ByteString -> B.Builder
lazyByteStringNul s = B.lazyByteString s <> nul

-- |Given a message, determine the (optional) type ID and the body
messageBody :: PGFrontendMessage -> (Maybe Char, B.Builder)
messageBody (StartupMessage kv) = (Nothing, B.word32BE 0x30000
  <> Fold.foldMap (\(k, v) -> byteStringNul k <> byteStringNul v) kv <> nul)
messageBody (CancelRequest pid key) = (Nothing, B.word32BE 80877102
  <> B.word32BE pid <> B.word32BE key)
messageBody Bind{ portalName = d, statementName = n, bindParameters = p, binaryColumns = bc } = (Just 'B',
  byteStringNul d
    <> byteStringNul n
    <> (if any fmt p
          then B.word16BE (fromIntegral $ length p) <> Fold.foldMap (B.word16BE . fromIntegral . fromEnum . fmt) p
          else B.word16BE 0)
    <> B.word16BE (fromIntegral $ length p) <> Fold.foldMap val p
    <> (if or bc
          then B.word16BE (fromIntegral $ length bc) <> Fold.foldMap (B.word16BE . fromIntegral . fromEnum) bc
          else B.word16BE 0))
  where
  fmt (PGBinaryValue _) = True
  fmt _ = False
  val PGNullValue = B.int32BE (-1)
  val (PGTextValue v) = B.word32BE (fromIntegral $ BS.length v) <> B.byteString v
  val (PGBinaryValue v) = B.word32BE (fromIntegral $ BS.length v) <> B.byteString v
messageBody CloseStatement{ statementName = n } = (Just 'C', 
  B.char7 'S' <> byteStringNul n)
messageBody ClosePortal{ portalName = n } = (Just 'C', 
  B.char7 'P' <> byteStringNul n)
messageBody DescribeStatement{ statementName = n } = (Just 'D',
  B.char7 'S' <> byteStringNul n)
messageBody DescribePortal{ portalName = n } = (Just 'D',
  B.char7 'P' <> byteStringNul n)
messageBody Execute{ portalName = n, executeRows = r } = (Just 'E',
  byteStringNul n <> B.word32BE r)
messageBody Flush = (Just 'H', mempty)
messageBody Parse{ statementName = n, queryString = s, parseTypes = t } = (Just 'P',
  byteStringNul n <> lazyByteStringNul s
    <> B.word16BE (fromIntegral $ length t) <> Fold.foldMap B.word32BE t)
messageBody (PasswordMessage s) = (Just 'p',
  B.byteString s <> nul)
messageBody SimpleQuery{ queryString = s } = (Just 'Q',
  lazyByteStringNul s)
messageBody Sync = (Just 'S', mempty)
messageBody Terminate = (Just 'X', mempty)

-- |Send a message to PostgreSQL (low-level).
pgSend :: PGConnection -> PGFrontendMessage -> IO ()
pgSend c@PGConnection{ connHandle = h, connState = sr } msg = do
  modifyIORef' sr $ state msg
  connDebugMsg c $ "> " ++ show msg
  pgPutBuilder h $ Fold.foldMap B.char7 t <> B.word32BE (fromIntegral $ 4 + BS.length b)
  pgPut h b -- or B.hPutBuilder? But we've already had to convert to BS to get length
  where
  (t, b) = second (BSL.toStrict . B.toLazyByteString) $ messageBody msg
  state _ StateClosed = StateClosed
  state Sync _ = StatePending
  state SimpleQuery{} _ = StatePending
  state Terminate _ = StateClosed
  state _ _ = StateUnsync


getByteStringNul :: G.Get BS.ByteString
getByteStringNul = fmap BSL.toStrict G.getLazyByteStringNul

getMessageFields :: G.Get MessageFields
getMessageFields = g . w2c =<< G.getWord8 where
  g '\0' = return Map.empty
  g f = liftM2 (Map.insert f) getByteStringNul getMessageFields

-- |Parse an incoming message.
getMessageBody :: Char -> G.Get PGBackendMessage
getMessageBody 'R' = auth =<< G.getWord32be where
  auth 0 = return AuthenticationOk
  auth 3 = return AuthenticationCleartextPassword
  auth 5 = AuthenticationMD5Password <$> G.getByteString 4
  auth op = fail $ "pgGetMessage: unsupported authentication type: " ++ show op
getMessageBody 't' = do
  numParams <- G.getWord16be
  ParameterDescription <$> replicateM (fromIntegral numParams) G.getWord32be
getMessageBody 'T' = do
  numFields <- G.getWord16be
  RowDescription <$> replicateM (fromIntegral numFields) getField where
  getField = do
    name <- getByteStringNul
    oid <- G.getWord32be -- table OID
    col <- G.getWord16be -- column number
    typ' <- G.getWord32be -- type
    siz <- G.getWord16be -- type size
    tmod <- G.getWord32be -- type modifier
    fmt <- G.getWord16be -- format code
    return $ PGColDescription
      { pgColName = name
      , pgColTable = oid
      , pgColNumber = fromIntegral col
      , pgColType = typ'
      , pgColSize = fromIntegral siz
      , pgColModifier = fromIntegral tmod
      , pgColBinary = toEnum (fromIntegral fmt)
      }
getMessageBody 'Z' = ReadyForQuery <$> (rs . w2c =<< G.getWord8) where
  rs 'I' = return StateIdle
  rs 'T' = return StateTransaction
  rs 'E' = return StateTransactionFailed
  rs s = fail $ "pgGetMessage: unknown ready state: " ++ show s
getMessageBody '1' = return ParseComplete
getMessageBody '2' = return BindComplete
getMessageBody '3' = return CloseComplete
getMessageBody 'C' = CommandComplete <$> getByteStringNul
getMessageBody 'S' = liftM2 ParameterStatus getByteStringNul getByteStringNul
getMessageBody 'D' = do 
  numFields <- G.getWord16be
  DataRow <$> replicateM (fromIntegral numFields) (getField =<< G.getWord32be) where
  getField 0xFFFFFFFF = return PGNullValue
  getField len = PGTextValue <$> G.getByteString (fromIntegral len)
  -- could be binary, too, but we don't know here, so have to choose one
getMessageBody 'K' = liftM2 BackendKeyData G.getWord32be G.getWord32be
getMessageBody 'E' = ErrorResponse <$> getMessageFields
getMessageBody 'I' = return EmptyQueryResponse
getMessageBody 'n' = return NoData
getMessageBody 's' = return PortalSuspended
getMessageBody 'N' = NoticeResponse <$> getMessageFields
getMessageBody 'A' = NotificationResponse <$> do
  PGNotification
    <$> G.getWord32be
    <*> getByteStringNul
    <*> G.getLazyByteStringNul
getMessageBody t = fail $ "pgGetMessage: unknown message type: " ++ show t

getMessage :: G.Decoder PGBackendMessage
getMessage = G.runGetIncremental $ do
  typ <- G.getWord8
  len <- G.getWord32be
  G.isolate (fromIntegral len - 4) $ getMessageBody (w2c typ)

class Show m => RecvMsg m where
  -- |Read from connection, returning immediate value or non-empty data
  recvMsgData :: PGConnection -> IO (Either m BS.ByteString)
  recvMsgData c = do
    r <- pgGetSome (connHandle c) smallChunkSize
    if BS.null r
      then do
        writeIORef (connState c) StateClosed
        pgCloseHandle (connHandle c)
        -- Should this instead be a special PGError?
        ioError $ mkIOError eofErrorType "PGConnection" Nothing Nothing
      else
        return (Right r)
  -- |Expected ReadyForQuery message
  recvMsgSync :: Maybe m
  recvMsgSync = Nothing
  -- |NotificationResponse message
  recvMsgNotif :: PGConnection -> PGNotification -> IO (Maybe m)
  recvMsgNotif c n = Nothing <$
    modifyIORef' (connNotifications c) (enQueue n)
  -- |ErrorResponse message
  recvMsgErr :: PGConnection -> MessageFields -> IO (Maybe m)
  recvMsgErr c m = Nothing <$
    connLogMessage c m
  -- |Any other unhandled message
  recvMsg :: PGConnection -> PGBackendMessage -> IO (Maybe m)
  recvMsg c m = Nothing <$ 
    connLogMessage c (makeMessage (BSC.pack $ "Unexpected server message: " ++ show m) "Each statement should only contain a single query")

-- |Process all pending messages
data RecvNonBlock = RecvNonBlock deriving (Show)
instance RecvMsg RecvNonBlock where
#ifndef mingw32_HOST_OS
  recvMsgData PGConnection{connHandle=PGSocket s} = do
    r <- recvNonBlock s smallChunkSize
    if BS.null r
      then return (Left RecvNonBlock)
      else return (Right r)
#else
  recvMsgData PGConnection{connHandle=PGSocket _} =
    throwIO (userError "Non-blocking recvMsgData is not supported on mingw32 ATM")
#endif
#ifdef VERSION_tls
  recvMsgData PGConnection{connHandle=PGTlsContext _} =
    throwIO (userError "Non-blocking recvMsgData is not supported on TLS connections")
#endif

-- |Wait for ReadyForQuery
data RecvSync = RecvSync deriving (Show)
instance RecvMsg RecvSync where
  recvMsgSync = Just RecvSync

-- |Wait for NotificationResponse
instance RecvMsg PGNotification where
  recvMsgNotif _ = return . Just

-- |Return any message (throwing errors)
instance RecvMsg PGBackendMessage where
  recvMsgErr _ = throwIO . PGError
  recvMsg _ = return . Just

-- |Return any message or ReadyForQuery
instance RecvMsg (Either PGBackendMessage RecvSync) where
  recvMsgSync = Just $ Right RecvSync
  recvMsgErr _ = throwIO . PGError
  recvMsg _ = return . Just . Left

-- |Receive the next message from PostgreSQL (low-level).
pgRecv :: RecvMsg m => PGConnection -> IO m
pgRecv c@PGConnection{ connInput = dr, connState = sr } =
  rcv =<< readIORef dr where
  next = writeIORef dr
  new = G.pushChunk getMessage

  -- read and parse
  rcv (G.Done b _ m) = do
    connDebugMsg c $ "< " ++ show m
    got (new b) m
  rcv (G.Fail _ _ r) = next (new BS.empty) >> fail r -- not clear how can recover
  rcv d@(G.Partial r) = recvMsgData c `onException` next d >>=
    either (<$ next d) (rcv . r . Just)

  -- process message
  msg (ParameterStatus k v) = Nothing <$
    modifyIORef' (connParameters c) (Map.insert k v)
  msg (NoticeResponse m) = Nothing <$
    connLogMessage c m
  msg (ErrorResponse m) =
    recvMsgErr c m
  msg m@(ReadyForQuery s) = do
    s' <- atomicModifyIORef' sr (s, )
    if s' == StatePending
      then return recvMsgSync -- expected
      else recvMsg c m -- unexpected
  msg (NotificationResponse n) =
    recvMsgNotif c n
  msg m@AuthenticationOk = do
    writeIORef sr StatePending
    recvMsg c m
  msg m = recvMsg c m
  got d m = msg m `onException` next d >>=
    maybe (rcv d) (<$ next d)

-- |Connect to a PostgreSQL server.
pgConnect :: PGDatabase -> IO PGConnection
pgConnect db = do
  param <- newIORef Map.empty
  state <- newIORef StateUnsync
  prepc <- newIORef 0
  prepm <- newIORef Map.empty
  input <- newIORef getMessage
  tr <- newIORef 0
  notif <- newIORef emptyQueue
  addr <- either
    (\(h,p) -> head <$> Net.getAddrInfo (Just defai) (Just h) (Just p))
    (\a -> return defai{ Net.addrAddress = a, Net.addrFamily = case a of
      Net.SockAddrInet{}  -> Net.AF_INET
      Net.SockAddrInet6{} -> Net.AF_INET6
      Net.SockAddrUnix{}  -> Net.AF_UNIX
      _ -> Net.AF_UNSPEC })
    $ pgDBAddr db
  sock <- Net.socket (Net.addrFamily addr) (Net.addrSocketType addr) (Net.addrProtocol addr)
  unless (Net.addrFamily addr == Net.AF_UNIX) $ Net.setSocketOption sock Net.NoDelay 1
  Net.connect sock $ Net.addrAddress addr
  pgHandle <- mkPGHandle db sock
  let c = PGConnection
        { connHandle = pgHandle
        , connDatabase = db
        , connPid = 0
        , connKey = 0
        , connParameters = param
        , connPreparedStatementCount = prepc
        , connPreparedStatementMap = prepm
        , connState = state
        , connTypeEnv = unknownPGTypeEnv
        , connInput = input
        , connTransaction = tr
        , connNotifications = notif
        }
  pgSend c $ StartupMessage $
    [ ("user", pgDBUser db)
    , ("database", pgDBName db)
    , ("client_encoding", "UTF8")
    , ("standard_conforming_strings", "on")
    , ("bytea_output", "hex")
    , ("DateStyle", "ISO, YMD")
    , ("IntervalStyle", "iso_8601")
    , ("extra_float_digits", "3")
    ] ++ pgDBParams db
  pgFlush c
  conn c
  where
  defai = Net.defaultHints{ Net.addrSocketType = Net.Stream }
  conn c = pgRecv c >>= msg c
  msg c (Right RecvSync) = do
    cp <- readIORef (connParameters c)
    return c
      { connTypeEnv = PGTypeEnv
        { pgIntegerDatetimes = fmap ("on" ==) $ Map.lookup "integer_datetimes" cp
        , pgServerVersion = Map.lookup "server_version" cp
        }
      }
  msg c (Left (BackendKeyData p k)) = conn c{ connPid = p, connKey = k }
  msg c (Left AuthenticationOk) = conn c
  msg c (Left AuthenticationCleartextPassword) = do
    pgSend c $ PasswordMessage $ pgDBPass db
    pgFlush c
    conn c
#ifdef VERSION_cryptonite
  msg c (Left (AuthenticationMD5Password salt)) = do
    pgSend c $ PasswordMessage $ "md5" `BS.append` md5 (md5 (pgDBPass db <> pgDBUser db) `BS.append` salt)
    pgFlush c
    conn c
#endif
  msg _ (Left m) = fail $ "pgConnect: unexpected response: " ++ show m

mkPGHandle :: PGDatabase -> Net.Socket -> IO PGHandle
#ifdef VERSION_tls
mkPGHandle db sock =
  case pgDBTLS db of
    TlsDisabled     -> pure (PGSocket sock)
    TlsNoValidate   -> mkTlsContext
    TlsValidate _ _ -> mkTlsContext
  where
    mkTlsContext = do
      NetBSL.sendAll sock sslRequest
      resp <- NetBS.recv sock 1
      case resp of
        "S" -> do
          ctx <- TLS.contextNew sock params
          void $ TLS.handshake ctx
          pure $ PGTlsContext ctx
        "N" -> throwIO (userError "Server does not support TLS")
        _ -> throwIO (userError "Unexpected response from server when issuing SSLRequest")
    params = (TLS.defaultParamsClient tlsHost tlsPort)
      { TLS.clientSupported =
          def { TLS.supportedCiphers = TLS.ciphersuite_strong }
      , TLS.clientShared = clientShared
      , TLS.clientHooks = clientHooks
      }
    tlsHost = case pgDBAddr db of
      Left (h,_) -> h
      Right (Net.SockAddrUnix s) -> s
      Right _ -> "some-socket"
    tlsPort = case pgDBAddr db of
      Left (_,p) -> BSC.pack p
      Right _    -> "socket"
    clientShared =
      case pgDBTLS db of
        TlsDisabled -> def { TLS.sharedValidationCache = noValidate }
        TlsNoValidate -> def { TLS.sharedValidationCache = noValidate }
        TlsValidate _ sc -> def { TLS.sharedCAStore = makeCertificateStore [sc] }
    clientHooks =
      case pgDBTLS db of
        TlsValidate TlsValidateCA _ -> def { TLS.onServerCertificate = validateNoCheckFQHN }
        _                           -> def
    validateNoCheckFQHN = Data.X509.Validation.validate HashSHA256 def (def { TLS.checkFQHN = False })

    noValidate = TLS.ValidationCache
      (\_ _ _ -> return TLS.ValidationCachePass)
      (\_ _ _ -> return ())
    sslRequest = B.toLazyByteString (B.word32BE 8 <> B.word32BE 80877103)
#else
mkPGHandle _ sock = pure (PGSocket sock)
#endif

-- |Disconnect cleanly from the PostgreSQL server.
pgDisconnect :: PGConnection -- ^ a handle from 'pgConnect'
             -> IO ()
pgDisconnect c@PGConnection{ connHandle = h } =
  pgSend c Terminate `finally` pgCloseHandle h

-- |Disconnect cleanly from the PostgreSQL server, but only if it's still connected.
pgDisconnectOnce :: PGConnection -- ^ a handle from 'pgConnect'
                 -> IO ()
pgDisconnectOnce c@PGConnection{ connState = cs } = do
  s <- readIORef cs
  unless (s == StateClosed) $
    pgDisconnect c

-- |Possibly re-open a connection to a different database, either reusing the connection if the given database is already connected or closing it and opening a new one.
-- Regardless, the input connection must not be used afterwards.
pgReconnect :: PGConnection -> PGDatabase -> IO PGConnection
pgReconnect c@PGConnection{ connDatabase = cd, connState = cs } d = do
  s <- readIORef cs
  if cd == d && s /= StateClosed
    then return c{ connDatabase = d }
    else do
      pgDisconnectOnce c
      pgConnect d

pgSync :: PGConnection -> IO ()
pgSync c@PGConnection{ connState = sr } = do
  s <- readIORef sr
  case s of
    StateClosed -> fail "pgSync: operation on closed connection"
    StatePending -> wait
    StateUnsync -> do
      pgSend c Sync
      pgFlush c
      wait
    _ -> return ()
  where
  wait = do
    RecvSync <- pgRecv c
    return ()
    
rowDescription :: PGBackendMessage -> PGRowDescription
rowDescription (RowDescription d) = d
rowDescription NoData = []
rowDescription m = error $ "describe: unexpected response: " ++ show m

-- |Describe a SQL statement/query. A statement description consists of 0 or
-- more parameter descriptions (a PostgreSQL type) and zero or more result
-- field descriptions (for queries) (consist of the name of the field, the
-- type of the field, and a nullability indicator).
pgDescribe :: PGConnection -> BSL.ByteString -- ^ SQL string
                  -> [OID] -- ^ Optional type specifications
                  -> Bool -- ^ Guess nullability, otherwise assume everything is
                  -> IO ([OID], [(BS.ByteString, OID, Bool)]) -- ^ a list of parameter types, and a list of result field names, types, and nullability indicators.
pgDescribe h sql types nulls = do
  pgSync h
  pgSend h Parse{ queryString = sql, statementName = BS.empty, parseTypes = types }
  pgSend h DescribeStatement{ statementName = BS.empty }
  pgSend h Sync
  pgFlush h
  ParseComplete <- pgRecv h
  ParameterDescription ps <- pgRecv h
  (,) ps <$> (mapM desc . rowDescription =<< pgRecv h)
  where
  desc (PGColDescription{ pgColName = name, pgColTable = tab, pgColNumber = col, pgColType = typ }) = do
    n <- nullable tab col
    return (name, typ, n)
  -- We don't get nullability indication from PostgreSQL, at least not directly.
  -- Without any hints, we have to assume that the result can be null and
  -- leave it up to the developer to figure it out.
  nullable oid col
    | nulls && oid /= 0 = do
      -- In cases where the resulting field is tracable to the column of a
      -- table, we can check there.
      (_, r) <- pgPreparedQuery h "SELECT attnotnull FROM pg_catalog.pg_attribute WHERE attrelid = $1 AND attnum = $2" [26, 21] [pgEncodeRep (oid :: OID), pgEncodeRep (col :: Int16)] []
      case r of
        [[s]] -> return $ not $ pgDecodeRep s
        [] -> return True
        _ -> fail $ "Failed to determine nullability of column #" ++ show col
    | otherwise = return True

rowsAffected :: (Integral i, Read i) => BS.ByteString -> i
rowsAffected = ra . BSC.words where
  ra [] = -1
  ra l = fromMaybe (-1) $ readMaybe $ BSC.unpack $ last l

-- Do we need to use the PGColDescription here always, or are the request formats okay?
fixBinary :: [Bool] -> PGValues -> PGValues
fixBinary (False:b) (PGBinaryValue x:r) = PGTextValue x : fixBinary b r
fixBinary (True :b) (PGTextValue x:r) = PGBinaryValue x : fixBinary b r
fixBinary (_:b) (x:r) = x : fixBinary b r
fixBinary _ l = l

-- |A simple query is one which requires sending only a single 'SimpleQuery'
-- message to the PostgreSQL server. The query is sent as a single string; you
-- cannot bind parameters. Note that queries can return 0 results (an empty
-- list).
pgSimpleQuery :: PGConnection -> BSL.ByteString -- ^ SQL string
                   -> IO (Int, [PGValues]) -- ^ The number of rows affected and a list of result rows
pgSimpleQuery h sql = do
  pgSync h
  pgSend h $ SimpleQuery sql
  pgFlush h
  go start where 
  go = (pgRecv h >>=)
  start (RowDescription rd) = go $ row (map pgColBinary rd) id
  start (CommandComplete c) = got c []
  start EmptyQueryResponse = return (0, [])
  start m = fail $ "pgSimpleQuery: unexpected response: " ++ show m
  row bc r (DataRow fs) = go $ row bc (r . (fixBinary bc fs :))
  row _ r (CommandComplete c) = got c (r [])
  row _ _ m = fail $ "pgSimpleQuery: unexpected row: " ++ show m
  got c r = return (rowsAffected c, r)

-- |A simple query which may contain multiple queries (separated by semi-colons) whose results are all ignored.
-- This function can also be used for \"SET\" parameter queries if necessary, but it's safer better to use 'pgDBParams'.
pgSimpleQueries_ :: PGConnection -> BSL.ByteString -- ^ SQL string
                   -> IO ()
pgSimpleQueries_ h sql = do
  pgSync h
  pgSend h $ SimpleQuery sql
  pgFlush h
  go where
  go = pgRecv h >>= res
  res (Left (RowDescription _)) = go
  res (Left (CommandComplete _)) = go
  res (Left EmptyQueryResponse) = go
  res (Left (DataRow _)) = go
  res (Right RecvSync) = return ()
  res m = fail $ "pgSimpleQueries_: unexpected response: " ++ show m

pgPreparedBind :: PGConnection -> BS.ByteString -> [OID] -> PGValues -> [Bool] -> IO (IO ())
pgPreparedBind c sql types bind bc = do
  pgSync c
  m <- readIORef (connPreparedStatementMap c)
  (p, n) <- maybe
    (atomicModifyIORef' (connPreparedStatementCount c) (succ &&& (,) False . PGPreparedStatement))
    (return . (,) True) $ Map.lookup key m
  unless p $
    pgSend c Parse{ queryString = BSL.fromStrict sql, statementName = preparedStatementName n, parseTypes = types }
  pgSend c Bind{ portalName = BS.empty, statementName = preparedStatementName n, bindParameters = bind, binaryColumns = bc }
  let
    go = pgRecv c >>= start
    start ParseComplete = do
      modifyIORef' (connPreparedStatementMap c) $
        Map.insert key n
      go
    start BindComplete = return ()
    start r = fail $ "pgPrepared: unexpected response: " ++ show r
  return go
  where key = (sql, types)

-- |Prepare a statement, bind it, and execute it.
-- If the given statement has already been prepared (and not yet closed) on this connection, it will be re-used.
pgPreparedQuery :: PGConnection -> BS.ByteString -- ^ SQL statement with placeholders
  -> [OID] -- ^ Optional type specifications (only used for first call)
  -> PGValues -- ^ Paremeters to bind to placeholders
  -> [Bool] -- ^ Requested binary format for result columns
  -> IO (Int, [PGValues])
pgPreparedQuery c sql types bind bc = do
  start <- pgPreparedBind c sql types bind bc
  pgSend c Execute{ portalName = BS.empty, executeRows = 0 }
  pgSend c Sync
  pgFlush c
  start
  go id
  where
  go r = pgRecv c >>= row r
  row r (DataRow fs) = go (r . (fixBinary bc fs :))
  row r (CommandComplete d) = return (rowsAffected d, r [])
  row r EmptyQueryResponse = return (0, r [])
  row _ m = fail $ "pgPreparedQuery: unexpected row: " ++ show m

-- |Like 'pgPreparedQuery' but requests results lazily in chunks of the given size.
-- Does not use a named portal, so other requests may not intervene.
pgPreparedLazyQuery :: PGConnection -> BS.ByteString -> [OID] -> PGValues -> [Bool] -> Word32 -- ^ Chunk size (1 is common, 0 is all-at-once)
  -> IO [PGValues]
pgPreparedLazyQuery c sql types bind bc count = do
  start <- pgPreparedBind c sql types bind bc
  unsafeInterleaveIO $ do
    execute
    start
    go id
  where
  execute = do
    pgSend c Execute{ portalName = BS.empty, executeRows = count }
    pgSend c Flush
    pgFlush c
  go r = pgRecv c >>= row r
  row r (DataRow fs) = go (r . (fixBinary bc fs :))
  row r PortalSuspended = r <$> unsafeInterleaveIO (execute >> go id)
  row r (CommandComplete _) = return (r [])
  row r EmptyQueryResponse = return (r [])
  row _ m = fail $ "pgPreparedLazyQuery: unexpected row: " ++ show m

-- |Close a previously prepared query (if necessary).
pgCloseStatement :: PGConnection -> BS.ByteString -> [OID] -> IO ()
pgCloseStatement c sql types = do
  mn <- atomicModifyIORef (connPreparedStatementMap c) $
    swap . Map.updateLookupWithKey (\_ _ -> Nothing) (sql, types)
  Fold.mapM_ (pgClose c) mn

-- |Begin a new transaction. If there is already a transaction in progress (created with 'pgBegin' or 'pgTransaction') instead creates a savepoint.
pgBegin :: PGConnection -> IO ()
pgBegin c@PGConnection{ connTransaction = tr } = do
  t <- atomicModifyIORef' tr (succ &&& id)
  void $ pgSimpleQuery c $ BSLC.pack $ if t == 0 then "BEGIN" else "SAVEPOINT pgt" ++ show t

predTransaction :: Word -> (Word, Word)
predTransaction 0 = (0, error "pgTransaction: no transactions")
predTransaction x = (x', x') where x' = pred x

-- |Rollback to the most recent 'pgBegin'.
pgRollback :: PGConnection -> IO ()
pgRollback c@PGConnection{ connTransaction = tr } = do
  t <- atomicModifyIORef' tr predTransaction
  void $ pgSimpleQuery c $ BSLC.pack $ if t == 0 then "ROLLBACK" else "ROLLBACK TO SAVEPOINT pgt" ++ show t

-- |Commit the most recent 'pgBegin'.
pgCommit :: PGConnection -> IO ()
pgCommit c@PGConnection{ connTransaction = tr } = do
  t <- atomicModifyIORef' tr predTransaction
  void $ pgSimpleQuery c $ BSLC.pack $ if t == 0 then "COMMIT" else "RELEASE SAVEPOINT pgt" ++ show t

-- |Rollback all active 'pgBegin's.
pgRollbackAll :: PGConnection -> IO ()
pgRollbackAll c@PGConnection{ connTransaction = tr } = do
  writeIORef tr 0
  void $ pgSimpleQuery c $ BSLC.pack "ROLLBACK"

-- |Commit all active 'pgBegin's.
pgCommitAll :: PGConnection -> IO ()
pgCommitAll c@PGConnection{ connTransaction = tr } = do
  writeIORef tr 0
  void $ pgSimpleQuery c $ BSLC.pack "COMMIT"

-- |Wrap a computation in a 'pgBegin', 'pgCommit' block, or 'pgRollback' on exception.
pgTransaction :: PGConnection -> IO a -> IO a
pgTransaction c f = do
  pgBegin c
  onException (do
    r <- f
    pgCommit c
    return r)
    (pgRollback c)

-- |Prepare, bind, execute, and close a single (unnamed) query, and return the number of rows affected, or Nothing if there are (ignored) result rows.
pgRun :: PGConnection -> BSL.ByteString -> [OID] -> PGValues -> IO (Maybe Integer)
pgRun c sql types bind = do
  pgSync c
  pgSend c Parse{ queryString = sql, statementName = BS.empty, parseTypes = types }
  pgSend c Bind{ portalName = BS.empty, statementName = BS.empty, bindParameters = bind, binaryColumns = [] }
  pgSend c Execute{ portalName = BS.empty, executeRows = 1 } -- 0 does not mean none
  pgSend c Sync
  pgFlush c
  go where
  go = pgRecv c >>= res
  res ParseComplete = go
  res BindComplete = go
  res (DataRow _) = go
  res PortalSuspended = return Nothing
  res (CommandComplete d) = return (Just $ rowsAffected d)
  res EmptyQueryResponse = return (Just 0)
  res m = fail $ "pgRun: unexpected response: " ++ show m

-- |Prepare a single query and return its handle.
pgPrepare :: PGConnection -> BSL.ByteString -> [OID] -> IO PGPreparedStatement
pgPrepare c sql types = do
  n <- atomicModifyIORef' (connPreparedStatementCount c) (succ &&& PGPreparedStatement)
  pgSync c
  pgSend c Parse{ queryString = sql, statementName = preparedStatementName n, parseTypes = types }
  pgSend c Sync
  pgFlush c
  ParseComplete <- pgRecv c
  return n

-- |Close a previously prepared query.
pgClose :: PGConnection -> PGPreparedStatement -> IO ()
pgClose c n = do
  pgSync c
  pgSend c ClosePortal{ portalName = preparedStatementName n }
  pgSend c CloseStatement{ statementName = preparedStatementName n }
  pgSend c Sync
  pgFlush c
  CloseComplete <- pgRecv c
  CloseComplete <- pgRecv c
  return ()

-- |Bind a prepared statement, and return the row description.
-- After 'pgBind', you must either call 'pgFetch' until it completes (returns @(_, 'Just' _)@) or 'pgFinish' before calling 'pgBind' again on the same prepared statement.
pgBind :: PGConnection -> PGPreparedStatement -> PGValues -> IO PGRowDescription
pgBind c n bind = do
  pgSync c
  pgSend c ClosePortal{ portalName = sn }
  pgSend c Bind{ portalName = sn, statementName = sn, bindParameters = bind, binaryColumns = [] }
  pgSend c DescribePortal{ portalName = sn }
  pgSend c Sync
  pgFlush c
  CloseComplete <- pgRecv c
  BindComplete <- pgRecv c
  rowDescription <$> pgRecv c
  where sn = preparedStatementName n

-- |Fetch some rows from an executed prepared statement, returning the next N result rows (if any) and number of affected rows when complete.
pgFetch :: PGConnection -> PGPreparedStatement -> Word32 -- ^Maximum number of rows to return, or 0 for all
  -> IO ([PGValues], Maybe Integer)
pgFetch c n count = do
  pgSync c
  pgSend c Execute{ portalName = preparedStatementName n, executeRows = count }
  pgSend c Sync
  pgFlush c
  go where
  go = pgRecv c >>= res
  res (DataRow v) = first (v :) <$> go
  res PortalSuspended = return ([], Nothing)
  res (CommandComplete d) = do
    pgSync c
    pgSend c ClosePortal{ portalName = preparedStatementName n }
    pgSend c Sync
    pgFlush c
    CloseComplete <- pgRecv c
    return ([], Just $ rowsAffected d)
  res EmptyQueryResponse = return ([], Just 0)
  res m = fail $ "pgFetch: unexpected response: " ++ show m

-- |Retrieve a notifications, blocking if necessary.
pgGetNotification :: PGConnection -> IO PGNotification
pgGetNotification c =
  maybe (pgRecv c) return
   =<< atomicModifyIORef' (connNotifications c) deQueue

-- |Retrieve any pending notifications.  Non-blocking.
pgGetNotifications :: PGConnection -> IO [PGNotification]
pgGetNotifications c = do
  RecvNonBlock <- pgRecv c
  queueToList <$> atomicModifyIORef' (connNotifications c) (emptyQueue, )
  where
  queueToList :: Queue a -> [a]
  queueToList (Queue e d) = d ++ reverse e


--TODO: Implement non-blocking recv on mingw32
#ifndef mingw32_HOST_OS
recvNonBlock
  :: Net.Socket        -- ^ Connected socket
  -> Int               -- ^ Maximum number of bytes to receive
  -> IO BS.ByteString  -- ^ Data received
recvNonBlock s nbytes
  | nbytes < 0 = ioError (mkInvalidRecvArgError "Database.PostgreSQL.Typed.Protocol.recvNonBlock")
  | otherwise  = createAndTrim nbytes $ \ptr -> recvBufNonBlock s ptr nbytes

recvBufNonBlock :: Net.Socket -> Ptr Word8 -> Int -> IO Int
recvBufNonBlock s ptr nbytes
 | nbytes <= 0 = ioError (mkInvalidRecvArgError "Database.PostgreSQL.Typed.recvBufNonBlock")
 | otherwise   = do
    len <-
#if MIN_VERSION_network(3,1,0)
      Net.withFdSocket s $ \fd ->
#elif MIN_VERSION_network(3,0,0)
      Net.fdSocket s >>= \fd ->
#else
      let fd = Net.fdSocket s in
#endif
        c_recv fd (castPtr ptr) (fromIntegral nbytes) 0
    if len == -1
      then do
        errno <- getErrno
        if errno == eWOULDBLOCK
          then return 0
          else throwIO (errnoToIOError "recvBufNonBlock" errno Nothing (Just "Database.PostgreSQL.Typed"))
      else
        return $ fromIntegral len

mkInvalidRecvArgError :: String -> IOError
mkInvalidRecvArgError loc = ioeSetErrorString (mkIOError
                                    InvalidArgument
                                    loc Nothing Nothing) "non-positive length"


foreign import ccall unsafe "recv"
  c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt
#endif
