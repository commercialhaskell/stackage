{-# LANGUAGE CPP, FlexibleInstances, ScopedTypeVariables, MultiParamTypeClasses, FlexibleContexts, DataKinds, KindSignatures, TypeFamilies, DeriveDataTypeable #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif
-- |
-- Module: Database.PostgreSQL.Typed.Types
-- Copyright: 2015 Dylan Simon
-- 
-- Classes to support type inference, value encoding/decoding, and instances to support built-in PostgreSQL types.

module Database.PostgreSQL.Typed.Types 
  (
  -- * Basic types
    OID
  , PGValue(..)
  , PGValues
  , PGTypeID(..)
  , PGTypeEnv(..), unknownPGTypeEnv
  , PGName(..), pgNameBS, pgNameString
  , PGRecord(..)

  -- * Marshalling classes
  , PGType(..)
  , PGParameter(..)
  , PGColumn(..)
  , PGStringType
  , PGRecordType

  -- * Marshalling interface
  , pgEncodeParameter
  , pgEscapeParameter
  , pgDecodeColumn
  , pgDecodeColumnNotNull

  -- * Conversion utilities
  , pgQuote
  , pgDQuote
  , pgDQuoteFrom
  , parsePGDQuote
  , buildPGValue
  ) where

import qualified Codec.Binary.UTF8.String as UTF8
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<$), (<*), (*>))
#endif
import Control.Arrow ((&&&))
#ifdef VERSION_aeson
import qualified Data.Aeson as JSON
#endif
import qualified Data.Attoparsec.ByteString as P (anyWord8)
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Builder.Prim as BSBP
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BSU
import Data.Char (isSpace, isDigit, digitToInt, intToDigit, toLower)
import Data.Data (Data)
import Data.Int
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mempty, mconcat)
#endif
import Data.Ratio ((%), numerator, denominator)
#ifdef VERSION_scientific
import Data.Scientific (Scientific)
#endif
import Data.String (IsString(..))
#ifdef VERSION_text
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
#endif
import qualified Data.Time as Time
#if MIN_VERSION_time(1,5,0)
import Data.Time (defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif
import Data.Typeable (Typeable)
#ifdef VERSION_uuid
import qualified Data.UUID as UUID
#endif
import Data.Word (Word8, Word32)
import GHC.TypeLits (Symbol, symbolVal, KnownSymbol)
import Numeric (readFloat)
#ifdef VERSION_postgresql_binary
#if MIN_VERSION_postgresql_binary(0,12,0)
import qualified PostgreSQL.Binary.Decoding as BinD
import qualified PostgreSQL.Binary.Encoding as BinE
#else
import qualified PostgreSQL.Binary.Decoder as BinD
import qualified PostgreSQL.Binary.Encoder as BinE
#endif
#endif

type PGTextValue = BS.ByteString
type PGBinaryValue = BS.ByteString
-- |A value passed to or from PostgreSQL in raw format.
data PGValue
  = PGNullValue
  | PGTextValue { pgTextValue :: PGTextValue } -- ^ The standard text encoding format (also used for unknown formats)
  | PGBinaryValue { pgBinaryValue :: PGBinaryValue } -- ^ Special binary-encoded data.  Not supported in all cases.
  deriving (Show, Eq)
-- |A list of (nullable) data values, e.g. a single row or query parameters.
type PGValues = [PGValue]

-- |Parameters that affect how marshalling happens.
-- Currenly we force all other relevant parameters at connect time.
-- Nothing values represent unknown.
data PGTypeEnv = PGTypeEnv
  { pgIntegerDatetimes :: Maybe Bool -- ^ If @integer_datetimes@ is @on@; only relevant for binary encoding.
  , pgServerVersion :: Maybe BS.ByteString -- ^ The @server_version@ parameter
  } deriving (Show)

unknownPGTypeEnv :: PGTypeEnv
unknownPGTypeEnv = PGTypeEnv
  { pgIntegerDatetimes = Nothing
  , pgServerVersion = Nothing
  }

-- |A PostgreSQL literal identifier, generally corresponding to the \"name\" type (63-byte strings), but as it would be entered in a query, so may include double-quoting for special characters or schema-qualification.
newtype PGName = PGName
  { pgNameBytes :: [Word8] -- ^Raw bytes of the identifier (should really be a 'BS.ByteString', but we need a working 'Data' instance for annotations).
  }
  deriving (Eq, Ord, Typeable, Data)

-- |The literal identifier as used in a query.
pgNameBS :: PGName -> BS.ByteString
pgNameBS = BS.pack . pgNameBytes

-- |Applies utf-8 encoding.
instance IsString PGName where
  fromString = PGName . UTF8.encode
-- |Unquoted 'pgNameString'.
instance Show PGName where
  show = pgNameString

-- |Reverses the 'IsString' instantce.
pgNameString :: PGName -> String
pgNameString = UTF8.decode . pgNameBytes

-- |A proxy type for PostgreSQL types.  The type argument should be an (internal) name of a database type, as per @format_type(OID)@ (usually the same as @\\dT+@).
-- When the type's namespace (schema) is not in @search_path@, this will be explicitly qualified, so you should be sure to have a consistent @search_path@ for all database connections.
-- The underlying 'Symbol' should be considered a lifted 'PGName'.
data PGTypeID (t :: Symbol) = PGTypeProxy

-- |A valid PostgreSQL type, its metadata, and corresponding Haskell representation.
-- For conversion the other way (from Haskell type to PostgreSQL), see 'Database.PostgreSQL.Typed.Dynamic.PGRep'.
-- Unfortunately any instances of this will be orphans.
class (KnownSymbol t
#if __GLASGOW_HASKELL__ >= 800
    , PGParameter t (PGVal t), PGColumn t (PGVal t)
#endif
    ) => PGType t where
  -- |The default, native Haskell representation of this type, which should be as close as possible to the PostgreSQL representation.
  type PGVal t :: *
  -- |The string name of this type: specialized version of 'symbolVal'.
  pgTypeName :: PGTypeID t -> PGName
  pgTypeName = fromString . symbolVal
  -- |Does this type support binary decoding?
  -- If so, 'pgDecodeBinary' must be implemented for every 'PGColumn' instance of this type.
  pgBinaryColumn :: PGTypeEnv -> PGTypeID t -> Bool
  pgBinaryColumn _ _ = False

-- |A @PGParameter t a@ instance describes how to encode a PostgreSQL type @t@ from @a@.
class PGType t => PGParameter t a where
  -- |Encode a value to a PostgreSQL text representation.
  pgEncode :: PGTypeID t -> a -> PGTextValue
  -- |Encode a value to a (quoted) literal value for use in SQL statements.
  -- Defaults to a quoted version of 'pgEncode'
  pgLiteral :: PGTypeID t -> a -> BS.ByteString
  pgLiteral t = pgQuote . pgEncode t
  -- |Encode a value to a PostgreSQL representation.
  -- Defaults to the text representation by pgEncode
  pgEncodeValue :: PGTypeEnv -> PGTypeID t -> a -> PGValue
  pgEncodeValue _ t = PGTextValue . pgEncode t

-- |A @PGColumn t a@ instance describes how te decode a PostgreSQL type @t@ to @a@.
class PGType t => PGColumn t a where
  -- |Decode the PostgreSQL text representation into a value.
  pgDecode :: PGTypeID t -> PGTextValue -> a
  -- |Decode the PostgreSQL binary representation into a value.
  -- Only needs to be implemented if 'pgBinaryColumn' is true.
  pgDecodeBinary :: PGTypeEnv -> PGTypeID t -> PGBinaryValue -> a
  pgDecodeBinary _ t _ = error $ "pgDecodeBinary " ++ show (pgTypeName t) ++ ": not supported"
  pgDecodeValue :: PGTypeEnv -> PGTypeID t -> PGValue -> a
  pgDecodeValue _ t (PGTextValue v) = pgDecode t v
  pgDecodeValue e t (PGBinaryValue v) = pgDecodeBinary e t v
  pgDecodeValue _ t PGNullValue = error $ "NULL in " ++ show (pgTypeName t) ++ " column (use Maybe or COALESCE)"

instance PGParameter t a => PGParameter t (Maybe a) where
  pgEncode t = maybe (error $ "pgEncode " ++ show (pgTypeName t) ++ ": Nothing") (pgEncode t)
  pgLiteral = maybe (BSC.pack "NULL") . pgLiteral
  pgEncodeValue e = maybe PGNullValue . pgEncodeValue e

instance PGColumn t a => PGColumn t (Maybe a) where
  pgDecode t = Just . pgDecode t
  pgDecodeBinary e t = Just . pgDecodeBinary e t
  pgDecodeValue _ _ PGNullValue = Nothing
  pgDecodeValue e t v = Just $ pgDecodeValue e t v

-- |Final parameter encoding function used when a (nullable) parameter is passed to a prepared query.
pgEncodeParameter :: PGParameter t a => PGTypeEnv -> PGTypeID t -> a -> PGValue
pgEncodeParameter = pgEncodeValue

-- |Final parameter escaping function used when a (nullable) parameter is passed to be substituted into a simple query.
pgEscapeParameter :: PGParameter t a => PGTypeEnv -> PGTypeID t -> a -> BS.ByteString
pgEscapeParameter _ = pgLiteral

-- |Final column decoding function used for a nullable result value.
pgDecodeColumn :: PGColumn t (Maybe a) => PGTypeEnv -> PGTypeID t -> PGValue -> Maybe a
pgDecodeColumn = pgDecodeValue

-- |Final column decoding function used for a non-nullable result value.
pgDecodeColumnNotNull :: PGColumn t a => PGTypeEnv -> PGTypeID t -> PGValue -> a
pgDecodeColumnNotNull = pgDecodeValue


pgQuoteUnsafe :: BS.ByteString -> BS.ByteString
pgQuoteUnsafe = (`BSC.snoc` '\'') . BSC.cons '\''

-- |Produce a SQL string literal by wrapping (and escaping) a string with single quotes.
pgQuote :: BS.ByteString -> BS.ByteString
pgQuote s
  | '\0' `BSC.elem` s = error "pgQuote: unhandled null in literal"
  | otherwise = pgQuoteUnsafe $ BSC.intercalate (BSC.pack "''") $ BSC.split '\'' s

-- |Shorthand for @'BSL.toStrict' . 'BSB.toLazyByteString'@
buildPGValue :: BSB.Builder -> BS.ByteString
buildPGValue = BSL.toStrict . BSB.toLazyByteString

-- |Double-quote a value (e.g., as an identifier).
-- Does not properly handle unicode escaping (yet).
pgDQuote :: BS.ByteString -> BSB.Builder
pgDQuote s = dq <> BSBP.primMapByteStringBounded ec s <> dq where
  dq = BSB.char7 '"'
  ec = BSBP.condB (\c -> c == c2w '"' || c == c2w '\\') bs (BSBP.liftFixedToBounded BSBP.word8)
  bs = BSBP.liftFixedToBounded $ ((,) '\\') BSBP.>$< (BSBP.char7 BSBP.>*< BSBP.word8)

-- |Double-quote a value if it's \"\", \"null\", or contains any whitespace, \'\"\', \'\\\', or the characters given in the first argument.
pgDQuoteFrom :: [Char] -> BS.ByteString -> BSB.Builder
pgDQuoteFrom unsafe s
  | BS.null s || BSC.any (\c -> isSpace c || c == '"' || c == '\\' || c `elem` unsafe) s || BSC.map toLower s == BSC.pack "null" = pgDQuote s
  | otherwise = BSB.byteString s

-- |Parse double-quoted values ala 'pgDQuote'.
parsePGDQuote :: Bool -> [Char] -> (BS.ByteString -> Bool) -> P.Parser (Maybe BS.ByteString)
parsePGDQuote blank unsafe isnul = (Just <$> q) <> (mnul <$> uq) where
  q = P.char '"' *> (BS.concat <$> qs)
  qs = do
    p <- P.takeTill (\c -> c == '"' || c == '\\')
    e <- P.anyChar
    if e == '"'
      then return [p]
      else do
        c <- P.anyWord8
        (p :) . (BS.singleton c :) <$> qs
  uq = (if blank then P.takeWhile else P.takeWhile1) (`notElem` ('"':'\\':unsafe))
  mnul s
    | isnul s = Nothing
    | otherwise = Just s

#ifdef VERSION_postgresql_binary
binEnc :: BinEncoder a -> a -> BS.ByteString
binEnc = (.)
#if MIN_VERSION_postgresql_binary(0,12,0)
  BinE.encodingBytes

type BinDecoder = BinD.Value
type BinEncoder a = a -> BinE.Encoding
#else
  buildPGValue

type BinDecoder = BinD.Decoder
type BinEncoder a = BinE.Encoder a
#endif

binDec :: PGType t => BinDecoder a -> PGTypeID t -> PGBinaryValue -> a
binDec d t = either (\e -> error $ "pgDecodeBinary " ++ show (pgTypeName t) ++ ": " ++ show e) id . 
#if MIN_VERSION_postgresql_binary(0,12,0)
  BinD.valueParser
#else
  BinD.run
#endif
  d

#define BIN_COL pgBinaryColumn _ _ = True
#define BIN_ENC(F) pgEncodeValue _ _ = PGBinaryValue . binEnc (F)
#define BIN_DEC(F) pgDecodeBinary _ = binDec (F)
#else
#define BIN_COL
#define BIN_ENC(F)
#define BIN_DEC(F)
#endif

instance PGType "any" where
  type PGVal "any" = PGValue
instance PGType t => PGColumn t PGValue where
  pgDecode _ = PGTextValue
  pgDecodeBinary _ _ = PGBinaryValue
  pgDecodeValue _ _ = id
instance PGParameter "any" PGValue where
  pgEncode _ (PGTextValue v) = v
  pgEncode _ PGNullValue = error "pgEncode any: NULL"
  pgEncode _ (PGBinaryValue _) = error "pgEncode any: binary"
  pgEncodeValue _ _ = id

instance PGType "void" where
  type PGVal "void" = ()
instance PGParameter "void" () where
  pgEncode _ _ = BSC.empty
instance PGColumn "void" () where
  pgDecode _ _ = ()
  pgDecodeBinary _ _ _ = ()
  pgDecodeValue _ _ _ = ()

instance PGType "boolean" where
  type PGVal "boolean" = Bool
  BIN_COL
instance PGParameter "boolean" Bool where
  pgEncode _ False = BSC.singleton 'f'
  pgEncode _ True = BSC.singleton 't'
  pgLiteral _ False = BSC.pack "false"
  pgLiteral _ True = BSC.pack "true"
  BIN_ENC(BinE.bool)
instance PGColumn "boolean" Bool where
  pgDecode _ s = case BSC.head s of
    'f' -> False
    't' -> True
    c -> error $ "pgDecode boolean: " ++ [c]
  BIN_DEC(BinD.bool)

type OID = Word32
instance PGType "oid" where
  type PGVal "oid" = OID
  BIN_COL
instance PGParameter "oid" OID where
  pgEncode _ = BSC.pack . show
  pgLiteral = pgEncode
  BIN_ENC(BinE.int4_word32)
instance PGColumn "oid" OID where
  pgDecode _ = read . BSC.unpack
  BIN_DEC(BinD.int)

instance PGType "smallint" where
  type PGVal "smallint" = Int16
  BIN_COL
instance PGParameter "smallint" Int16 where
  pgEncode _ = BSC.pack . show
  pgLiteral = pgEncode
  BIN_ENC(BinE.int2_int16)
instance PGColumn "smallint" Int16 where
  pgDecode _ = read . BSC.unpack
  BIN_DEC(BinD.int)

instance PGType "integer" where 
  type PGVal "integer" = Int32
  BIN_COL
instance PGParameter "integer" Int32 where
  pgEncode _ = BSC.pack . show
  pgLiteral = pgEncode
  BIN_ENC(BinE.int4_int32)
instance PGColumn "integer" Int32 where
  pgDecode _ = read . BSC.unpack
  BIN_DEC(BinD.int)

instance PGType "bigint" where
  type PGVal "bigint" = Int64
  BIN_COL
instance PGParameter "bigint" Int64 where
  pgEncode _ = BSC.pack . show
  pgLiteral = pgEncode
  BIN_ENC(BinE.int8_int64)
instance PGColumn "bigint" Int64 where
  pgDecode _ = read . BSC.unpack
  BIN_DEC(BinD.int)

instance PGType "real" where
  type PGVal "real" = Float
  BIN_COL
instance PGParameter "real" Float where
  pgEncode _ = BSC.pack . show
  pgLiteral = pgEncode
  BIN_ENC(BinE.float4)
instance PGColumn "real" Float where
  pgDecode _ = read . BSC.unpack
  BIN_DEC(BinD.float4)
instance PGColumn "real" Double where
  pgDecode _ = read . BSC.unpack
  BIN_DEC(realToFrac <$> BinD.float4)

instance PGType "double precision" where
  type PGVal "double precision" = Double
  BIN_COL
instance PGParameter "double precision" Double where
  pgEncode _ = BSC.pack . show
  pgLiteral = pgEncode
  BIN_ENC(BinE.float8)
instance PGParameter "double precision" Float where
  pgEncode _ = BSC.pack . show
  pgLiteral = pgEncode
  BIN_ENC(BinE.float8 . realToFrac)
instance PGColumn "double precision" Double where
  pgDecode _ = read . BSC.unpack
  BIN_DEC(BinD.float8)

-- XXX need real encoding as text
-- but then no one should be using this type really...
instance PGType "\"char\"" where
  type PGVal "\"char\"" = Word8
  BIN_COL
instance PGParameter "\"char\"" Word8 where
  pgEncode _ = BS.singleton
  pgEncodeValue _ _ = PGBinaryValue . BS.singleton
instance PGColumn "\"char\"" Word8 where
  pgDecode _ = BS.head
  pgDecodeBinary _ _ = BS.head
instance PGParameter "\"char\"" Char where
  pgEncode _ = BSC.singleton
  pgEncodeValue _ _ = PGBinaryValue . BSC.singleton
instance PGColumn "\"char\"" Char where
  pgDecode _ = BSC.head
  pgDecodeBinary _ _ = BSC.head


class PGType t => PGStringType t

instance PGStringType t => PGParameter t String where
  pgEncode _ = BSU.fromString
  BIN_ENC(BinE.text_strict . T.pack)
instance PGStringType t => PGColumn t String where
  pgDecode _ = BSU.toString
  BIN_DEC(T.unpack <$> BinD.text_strict)

instance
#if __GLASGOW_HASKELL__ >= 710
    {-# OVERLAPPABLE #-}
#endif
    PGStringType t => PGParameter t BS.ByteString where
  pgEncode _ = id
  BIN_ENC(BinE.text_strict . TE.decodeUtf8)
instance
#if __GLASGOW_HASKELL__ >= 710
    {-# OVERLAPPABLE #-}
#endif
    PGStringType t => PGColumn t BS.ByteString where
  pgDecode _ = id
  BIN_DEC(TE.encodeUtf8 <$> BinD.text_strict)

instance
#if __GLASGOW_HASKELL__ >= 710
    {-# OVERLAPPABLE #-}
#endif
    PGStringType t => PGParameter t PGName where
  pgEncode _ = pgNameBS
  BIN_ENC(BinE.text_strict . TE.decodeUtf8 . pgNameBS)
instance
#if __GLASGOW_HASKELL__ >= 710
    {-# OVERLAPPABLE #-}
#endif
    PGStringType t => PGColumn t PGName where
  pgDecode _ = PGName . BS.unpack
  BIN_DEC(PGName . BS.unpack . TE.encodeUtf8 <$> BinD.text_strict)

instance
#if __GLASGOW_HASKELL__ >= 710
    {-# OVERLAPPABLE #-}
#endif
    PGStringType t => PGParameter t BSL.ByteString where
  pgEncode _ = BSL.toStrict
  BIN_ENC(BinE.text_lazy . TLE.decodeUtf8)
instance
#if __GLASGOW_HASKELL__ >= 710
    {-# OVERLAPPABLE #-}
#endif
    PGStringType t => PGColumn t BSL.ByteString where
  pgDecode _ = BSL.fromStrict
  BIN_DEC(TLE.encodeUtf8 <$> BinD.text_lazy)

#ifdef VERSION_text
instance PGStringType t => PGParameter t T.Text where
  pgEncode _ = TE.encodeUtf8
  BIN_ENC(BinE.text_strict)
instance PGStringType t => PGColumn t T.Text where
  pgDecode _ = TE.decodeUtf8
  BIN_DEC(BinD.text_strict)

instance PGStringType t => PGParameter t TL.Text where
  pgEncode _ = BSL.toStrict . TLE.encodeUtf8
  BIN_ENC(BinE.text_lazy)
instance PGStringType t => PGColumn t TL.Text where
  pgDecode _ = TL.fromStrict . TE.decodeUtf8
  BIN_DEC(BinD.text_lazy)
#define PGVALSTRING T.Text
#else
#define PGVALSTRING String
#endif

instance PGType "text" where
  type PGVal "text" = PGVALSTRING
  BIN_COL
instance PGType "character varying" where
  type PGVal "character varying" = PGVALSTRING
  BIN_COL
instance PGType "name" where
  type PGVal "name" = PGVALSTRING
  BIN_COL
instance PGType "bpchar" where
  type PGVal "bpchar" = PGVALSTRING
  BIN_COL
instance PGStringType "text"
instance PGStringType "character varying"
instance PGStringType "name" -- limit 63 characters; not strictly textsend but essentially the same
instance PGStringType "bpchar" -- blank padded


encodeBytea :: BSB.Builder -> PGTextValue
encodeBytea h = buildPGValue $ BSB.string7 "\\x" <> h

decodeBytea :: PGTextValue -> [Word8]
decodeBytea s
  | sm /= "\\x" = error $ "pgDecode bytea: " ++ sm
  | otherwise = pd $ BS.unpack d where
  (m, d) = BS.splitAt 2 s
  sm = BSC.unpack m
  pd [] = []
  pd (h:l:r) = (shiftL (unhex h) 4 .|. unhex l) : pd r
  pd [x] = error $ "pgDecode bytea: " ++ show x
  unhex = fromIntegral . digitToInt . w2c

instance PGType "bytea" where
  type PGVal "bytea" = BS.ByteString
  BIN_COL
instance
#if __GLASGOW_HASKELL__ >= 710
    {-# OVERLAPPING #-}
#endif
    PGParameter "bytea" BSL.ByteString where
  pgEncode _ = encodeBytea . BSB.lazyByteStringHex
  pgLiteral t = pgQuoteUnsafe . pgEncode t
  BIN_ENC(BinE.bytea_lazy)
instance
#if __GLASGOW_HASKELL__ >= 710
    {-# OVERLAPPING #-}
#endif
    PGColumn "bytea" BSL.ByteString where
  pgDecode _ = BSL.pack . decodeBytea
  BIN_DEC(BinD.bytea_lazy)
instance
#if __GLASGOW_HASKELL__ >= 710
    {-# OVERLAPPING #-}
#endif
    PGParameter "bytea" BS.ByteString where
  pgEncode _ = encodeBytea . BSB.byteStringHex
  pgLiteral t = pgQuoteUnsafe . pgEncode t
  BIN_ENC(BinE.bytea_strict)
instance
#if __GLASGOW_HASKELL__ >= 710
    {-# OVERLAPPING #-}
#endif
    PGColumn "bytea" BS.ByteString where
  pgDecode _ = BS.pack . decodeBytea
  BIN_DEC(BinD.bytea_strict)

readTime :: Time.ParseTime t => String -> String -> t
readTime =
#if MIN_VERSION_time(1,5,0)
  Time.parseTimeOrError False
#else
  Time.readTime
#endif
    defaultTimeLocale

instance PGType "date" where
  type PGVal "date" = Time.Day
  BIN_COL
instance PGParameter "date" Time.Day where
  pgEncode _ = BSC.pack . Time.showGregorian
  pgLiteral t = pgQuoteUnsafe . pgEncode t
  BIN_ENC(BinE.date)
instance PGColumn "date" Time.Day where
  pgDecode _ = readTime "%F" . BSC.unpack
  BIN_DEC(BinD.date)

binColDatetime :: PGTypeEnv -> PGTypeID t -> Bool
#ifdef VERSION_postgresql_binary
binColDatetime PGTypeEnv{ pgIntegerDatetimes = Just _ } _ = True
#endif
binColDatetime _ _ = False

#ifdef VERSION_postgresql_binary
binEncDatetime :: PGParameter t a => BinEncoder a -> BinEncoder a -> PGTypeEnv -> PGTypeID t -> a -> PGValue
binEncDatetime _ ff PGTypeEnv{ pgIntegerDatetimes = Just False } _ = PGBinaryValue . binEnc ff
binEncDatetime fi _ PGTypeEnv{ pgIntegerDatetimes = Just True } _ = PGBinaryValue . binEnc fi
binEncDatetime _ _ PGTypeEnv{ pgIntegerDatetimes = Nothing } t = PGTextValue . pgEncode t

binDecDatetime :: PGColumn t a => BinDecoder a -> BinDecoder a -> PGTypeEnv -> PGTypeID t -> PGBinaryValue -> a
binDecDatetime _ ff PGTypeEnv{ pgIntegerDatetimes = Just False } = binDec ff
binDecDatetime fi _ PGTypeEnv{ pgIntegerDatetimes = Just True } = binDec fi
binDecDatetime _ _ PGTypeEnv{ pgIntegerDatetimes = Nothing } = error "pgDecodeBinary: unknown integer_datetimes value"
#endif

-- PostgreSQL uses "[+-]HH[:MM]" timezone offsets, while "%z" uses "+HHMM" by default.
-- readTime can successfully parse both formats, but PostgreSQL needs the colon.
fixTZ :: String -> String
fixTZ "" = ""
fixTZ ['+',h1,h2] | isDigit h1 && isDigit h2 = ['+',h1,h2,':','0','0']
fixTZ ['-',h1,h2] | isDigit h1 && isDigit h2 = ['-',h1,h2,':','0','0']
fixTZ ['+',h1,h2,m1,m2] | isDigit h1 && isDigit h2 && isDigit m1 && isDigit m2 = ['+',h1,h2,':',m1,m2]
fixTZ ['-',h1,h2,m1,m2] | isDigit h1 && isDigit h2 && isDigit m1 && isDigit m2 = ['-',h1,h2,':',m1,m2]
fixTZ (c:s) = c:fixTZ s

instance PGType "time without time zone" where
  type PGVal "time without time zone" = Time.TimeOfDay
  pgBinaryColumn = binColDatetime
instance PGParameter "time without time zone" Time.TimeOfDay where
  pgEncode _ = BSC.pack . Time.formatTime defaultTimeLocale "%T%Q"
  pgLiteral t = pgQuoteUnsafe . pgEncode t
#ifdef VERSION_postgresql_binary
  pgEncodeValue = binEncDatetime BinE.time_int BinE.time_float
#endif
instance PGColumn "time without time zone" Time.TimeOfDay where
  pgDecode _ = readTime "%T%Q" . BSC.unpack
#ifdef VERSION_postgresql_binary
  pgDecodeBinary = binDecDatetime BinD.time_int BinD.time_float
#endif

instance PGType "time with time zone" where
  type PGVal "time with time zone" = (Time.TimeOfDay, Time.TimeZone)
  pgBinaryColumn = binColDatetime
instance PGParameter "time with time zone" (Time.TimeOfDay, Time.TimeZone) where
  pgEncode _ (t, z) = BSC.pack $ Time.formatTime defaultTimeLocale "%T%Q" t ++ fixTZ (Time.formatTime defaultTimeLocale "%z" z)
  pgLiteral t = pgQuoteUnsafe . pgEncode t
#ifdef VERSION_postgresql_binary
  pgEncodeValue = binEncDatetime BinE.timetz_int BinE.timetz_float
#endif
instance PGColumn "time with time zone" (Time.TimeOfDay, Time.TimeZone) where
  pgDecode _ = (Time.localTimeOfDay . Time.zonedTimeToLocalTime &&& Time.zonedTimeZone) . readTime "%T%Q%z" . fixTZ . BSC.unpack
#ifdef VERSION_postgresql_binary
  pgDecodeBinary = binDecDatetime BinD.timetz_int BinD.timetz_float
#endif

instance PGType "timestamp without time zone" where
  type PGVal "timestamp without time zone" = Time.LocalTime
  pgBinaryColumn = binColDatetime
instance PGParameter "timestamp without time zone" Time.LocalTime where
  pgEncode _ = BSC.pack . Time.formatTime defaultTimeLocale "%F %T%Q"
  pgLiteral t = pgQuoteUnsafe . pgEncode t
#ifdef VERSION_postgresql_binary
  pgEncodeValue = binEncDatetime BinE.timestamp_int BinE.timestamp_float
#endif
instance PGColumn "timestamp without time zone" Time.LocalTime where
  pgDecode _ = readTime "%F %T%Q" . BSC.unpack
#ifdef VERSION_postgresql_binary
  pgDecodeBinary = binDecDatetime BinD.timestamp_int BinD.timestamp_float
#endif

instance PGType "timestamp with time zone" where
  type PGVal "timestamp with time zone" = Time.UTCTime
  pgBinaryColumn = binColDatetime
instance PGParameter "timestamp with time zone" Time.UTCTime where
  pgEncode _ = BSC.pack . fixTZ . Time.formatTime defaultTimeLocale "%F %T%Q%z"
  -- pgLiteral t = pgQuoteUnsafe . pgEncode t
#ifdef VERSION_postgresql_binary
  pgEncodeValue = binEncDatetime BinE.timestamptz_int BinE.timestamptz_float
#endif
instance PGColumn "timestamp with time zone" Time.UTCTime where
  pgDecode _ = readTime "%F %T%Q%z" . fixTZ . BSC.unpack
#ifdef VERSION_postgresql_binary
  pgDecodeBinary = binDecDatetime BinD.timestamptz_int BinD.timestamptz_float
#endif

instance PGType "interval" where
  type PGVal "interval" = Time.DiffTime
  pgBinaryColumn = binColDatetime
instance PGParameter "interval" Time.DiffTime where
  pgEncode _ = BSC.pack . show
  pgLiteral t = pgQuoteUnsafe . pgEncode t
#ifdef VERSION_postgresql_binary
  pgEncodeValue = binEncDatetime BinE.interval_int BinE.interval_float
#endif
-- |Representation of DiffTime as interval.
-- PostgreSQL stores months and days separately in intervals, but DiffTime does not.
-- We collapse all interval fields into seconds
instance PGColumn "interval" Time.DiffTime where
  pgDecode _ a = either (error . ("pgDecode interval (" ++) . (++ ("): " ++ BSC.unpack a))) realToFrac $ P.parseOnly ps a where
    ps = do
      _ <- P.char 'P'
      d <- units [('Y', 12*month), ('M', month), ('W', 7*day), ('D', day)]
      ((d +) <$> pt) <> (d <$ P.endOfInput)
    pt = do
      _ <- P.char 'T'
      t <- units [('H', 3600), ('M', 60), ('S', 1)]
      P.endOfInput
      return t
    units l = fmap sum $ P.many' $ do
      x <- P.signed P.scientific
      u <- P.choice $ map (\(c, u) -> u <$ P.char c) l
      return $ x * u
    day = 86400
    month = 2629746
#ifdef VERSION_postgresql_binary
  pgDecodeBinary = binDecDatetime BinD.interval_int BinD.interval_float
#endif

instance PGType "numeric" where
  type PGVal "numeric" = 
#ifdef VERSION_scientific
    Scientific
#else
    Rational
#endif
  BIN_COL
instance PGParameter "numeric" Rational where
  pgEncode _ r
    | denominator r == 0 = BSC.pack "NaN" -- this can't happen
    | otherwise = BSC.pack $ take 30 (showRational (r / (10 ^^ e))) ++ 'e' : show e where
    e = floor $ logBase (10 :: Double) $ fromRational $ abs r :: Int -- not great, and arbitrarily truncate somewhere
  pgLiteral _ r
    | denominator r == 0 = BSC.pack "'NaN'" -- this can't happen
    | otherwise = BSC.pack $ '(' : show (numerator r) ++ '/' : show (denominator r) ++ "::numeric)"
  BIN_ENC(BinE.numeric . realToFrac)
-- |High-precision representation of Rational as numeric.
-- Unfortunately, numeric has an NaN, while Rational does not.
-- NaN numeric values will produce exceptions.
instance PGColumn "numeric" Rational where
  pgDecode _ bs
    | s == "NaN" = 0 % 0 -- this won't work
    | otherwise = ur $ readFloat s where
    ur [(x,"")] = x
    ur _ = error $ "pgDecode numeric: " ++ s
    s = BSC.unpack bs
  BIN_DEC(realToFrac <$> BinD.numeric)

-- This will produce infinite(-precision) strings
showRational :: Rational -> String
showRational r = show (ri :: Integer) ++ '.' : frac (abs rf) where
  (ri, rf) = properFraction r
  frac 0 = ""
  frac f = intToDigit i : frac f' where (i, f') = properFraction (10 * f)

#ifdef VERSION_scientific
instance PGParameter "numeric" Scientific where
  pgEncode _ = BSC.pack . show
  pgLiteral = pgEncode
  BIN_ENC(BinE.numeric)
instance PGColumn "numeric" Scientific where
  pgDecode _ = read . BSC.unpack
  BIN_DEC(BinD.numeric)
#endif

#ifdef VERSION_uuid
instance PGType "uuid" where
  type PGVal "uuid" = UUID.UUID
  BIN_COL
instance PGParameter "uuid" UUID.UUID where
  pgEncode _ = UUID.toASCIIBytes
  pgLiteral t = pgQuoteUnsafe . pgEncode t
  BIN_ENC(BinE.uuid)
instance PGColumn "uuid" UUID.UUID where
  pgDecode _ u = fromMaybe (error $ "pgDecode uuid: " ++ BSC.unpack u) $ UUID.fromASCIIBytes u
  BIN_DEC(BinD.uuid)
#endif

-- |Generic class of composite (row or record) types.
newtype PGRecord = PGRecord [Maybe PGTextValue]
class PGType t => PGRecordType t
instance PGRecordType t => PGParameter t PGRecord where
  pgEncode _ (PGRecord l) =
    buildPGValue $ BSB.char7 '(' <> mconcat (intersperse (BSB.char7 ',') $ map (maybe mempty (pgDQuoteFrom "(),")) l) <> BSB.char7 ')'
  pgLiteral _ (PGRecord l) =
    BSC.pack "ROW(" <> BS.intercalate (BSC.singleton ',') (map (maybe (BSC.pack "NULL") pgQuote) l) `BSC.snoc` ')'
instance PGRecordType t => PGColumn t PGRecord where
  pgDecode _ a = either (error . ("pgDecode record (" ++) . (++ ("): " ++ BSC.unpack a))) PGRecord $ P.parseOnly pa a where
    pa = P.char '(' *> P.sepBy el (P.char ',') <* P.char ')' <* P.endOfInput
    el = parsePGDQuote True "()," BS.null

instance PGType "record" where
  type PGVal "record" = PGRecord
-- |The generic anonymous record type, as created by @ROW@.
-- In this case we can not know the types, and in fact, PostgreSQL does not accept values of this type regardless (except as literals).
instance PGRecordType "record"

#ifdef VERSION_aeson
instance PGType "json" where
  type PGVal "json" = JSON.Value
  BIN_COL
instance PGParameter "json" JSON.Value where
  pgEncode _ = BSL.toStrict . JSON.encode
  BIN_ENC(BinE.json_ast)
instance PGColumn "json" JSON.Value where
  pgDecode _ j = either (error . ("pgDecode json (" ++) . (++ ("): " ++ BSC.unpack j))) id $ P.parseOnly JSON.json j
  BIN_DEC(BinD.json_ast)

instance PGType "jsonb" where
  type PGVal "jsonb" = JSON.Value
  BIN_COL
instance PGParameter "jsonb" JSON.Value where
  pgEncode _ = BSL.toStrict . JSON.encode
  BIN_ENC(BinE.jsonb_ast)
instance PGColumn "jsonb" JSON.Value where
  pgDecode _ j = either (error . ("pgDecode jsonb (" ++) . (++ ("): " ++ BSC.unpack j))) id $ P.parseOnly JSON.json j
  BIN_DEC(BinD.jsonb_ast)
#endif

{-
--, ( 142,  143, "xml",         ?)
--, ( 600, 1017, "point",       ?)
--, ( 650,  651, "cidr",        ?)
--, ( 790,  791, "money",       Centi? Fixed?)
--, ( 829, 1040, "macaddr",     ?)
--, ( 869, 1041, "inet",        ?)
--, (1266, 1270, "timetz",      ?)
--, (1560, 1561, "bit",         Bool?)
--, (1562, 1563, "varbit",      ?)
-}
