{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, DataKinds, DefaultSignatures, TemplateHaskell, TypeFamilies #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif
-- |
-- Module: Database.PostgreSQL.Typed.Dynamic
-- Copyright: 2015 Dylan Simon
-- 
-- Automatic (dynamic) marshalling of PostgreSQL values based on Haskell types (not SQL statements).
-- This is intended for direct construction of queries and query data, bypassing the normal SQL type inference.

module Database.PostgreSQL.Typed.Dynamic 
  ( PGRep(..)
  , pgTypeOf
  , pgTypeOfProxy
  , pgEncodeRep
  , pgDecodeRep
  , pgLiteralRep
  , pgLiteralString
  , pgSafeLiteral
  , pgSafeLiteralString
  , pgSubstituteLiterals
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
#ifdef VERSION_aeson
import qualified Data.Aeson as JSON
#endif
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Internal (w2c)
import qualified Data.ByteString.Lazy as BSL
import Data.Int
import Data.Monoid ((<>))
import Data.Proxy (Proxy)
#ifdef VERSION_scientific
import Data.Scientific (Scientific)
#endif
import Data.String (fromString)
#ifdef VERSION_text
import qualified Data.Text as T
#endif
import qualified Data.Time as Time
#ifdef VERSION_uuid
import qualified Data.UUID as UUID
#endif
import GHC.TypeLits (Symbol)
import Language.Haskell.Meta.Parse (parseExp)
import qualified Language.Haskell.TH as TH

import Database.PostgreSQL.Typed.Types
import Database.PostgreSQL.Typed.SQLToken

-- |Represents canonical/default PostgreSQL representation for various Haskell types, allowing convenient type-driven marshalling.
class (PGParameter (PGRepType a) a, PGColumn (PGRepType a) a) => PGRep a where
  -- |The PostgreSOL type that this type should be converted to.
  type PGRepType a :: Symbol

pgTypeOf :: a -> PGTypeID (PGRepType a)
pgTypeOf _ = PGTypeProxy

pgTypeOfProxy :: Proxy a -> PGTypeID (PGRepType a)
pgTypeOfProxy _ = PGTypeProxy

-- |Encode a value using 'pgEncodeValue'.
pgEncodeRep :: PGRep a => a -> PGValue
pgEncodeRep x = pgEncodeValue unknownPGTypeEnv (pgTypeOf x) x

-- |Produce a literal value for interpolation in a SQL statement using 'pgLiteral'.  Using 'pgSafeLiteral' is usually safer as it includes type cast.
pgLiteralRep :: PGRep a => a -> BS.ByteString
pgLiteralRep x = pgLiteral (pgTypeOf x) x

-- |Decode a value using 'pgDecodeValue'.
pgDecodeRep :: forall a . PGRep a => PGValue -> a
pgDecodeRep = pgDecodeValue unknownPGTypeEnv (PGTypeProxy :: PGTypeID (PGRepType a))

-- |Produce a raw SQL literal from a value. Using 'pgSafeLiteral' is usually safer when interpolating in a SQL statement.
pgLiteralString :: PGRep a => a -> String
pgLiteralString = BSC.unpack . pgLiteralRep

-- |Produce a safely type-cast literal value for interpolation in a SQL statement, e.g., "'123'::integer".
pgSafeLiteral :: PGRep a => a -> BS.ByteString
pgSafeLiteral x = pgLiteralRep x <> BSC.pack "::" <> pgNameBS (pgTypeName (pgTypeOf x))

-- |Identical to @'BSC.unpack' . 'pgSafeLiteral'@ but more efficient.
pgSafeLiteralString :: PGRep a => a -> String
pgSafeLiteralString x = pgLiteralString x ++ "::" ++ map w2c (pgNameBytes (pgTypeName (pgTypeOf x)))

instance PGRep a => PGRep (Maybe a) where
  type PGRepType (Maybe a) = PGRepType a

instance PGRep () where
  type PGRepType () = "void"
instance PGRep Bool where
  type PGRepType Bool = "boolean"
instance PGRep OID where
  type PGRepType OID = "oid"
instance PGRep Int16 where
  type PGRepType Int16 = "smallint"
instance PGRep Int32 where
  type PGRepType Int32 = "integer"
instance PGRep Int64 where
  type PGRepType Int64 = "bigint"
instance PGRep Float where
  type PGRepType Float = "real"
instance PGRep Double where
  type PGRepType Double = "double precision"
instance PGRep Char where
  type PGRepType Char = "\"char\""
instance PGRep String where
  type PGRepType String = "text"
instance PGRep BS.ByteString where
  type PGRepType BS.ByteString = "text"
instance PGRep PGName where
  type PGRepType PGName = "text" -- superset of "name"
#ifdef VERSION_text
instance PGRep T.Text where
  type PGRepType T.Text = "text"
#endif
instance PGRep Time.Day where
  type PGRepType Time.Day = "date"
instance PGRep Time.TimeOfDay where
  type PGRepType Time.TimeOfDay = "time without time zone"
instance PGRep (Time.TimeOfDay, Time.TimeZone) where
  type PGRepType (Time.TimeOfDay, Time.TimeZone) = "time with time zone"
instance PGRep Time.LocalTime where
  type PGRepType Time.LocalTime = "timestamp without time zone"
instance PGRep Time.UTCTime where
  type PGRepType Time.UTCTime = "timestamp with time zone"
instance PGRep Time.DiffTime where
  type PGRepType Time.DiffTime = "interval"
instance PGRep Rational where
  type PGRepType Rational = "numeric"
#ifdef VERSION_scientific
instance PGRep Scientific where
  type PGRepType Scientific = "numeric"
#endif
#ifdef VERSION_uuid
instance PGRep UUID.UUID where
  type PGRepType UUID.UUID = "uuid"
#endif
#ifdef VERSION_aeson
instance PGRep JSON.Value where
  type PGRepType JSON.Value = "jsonb"
#endif

-- |Create an expression that literally substitutes each instance of @${expr}@ for the result of @pgSafeLiteral expr@, producing a lazy 'BSL.ByteString'.
-- This lets you do safe, type-driven literal substitution into SQL fragments without needing a full query, bypassing placeholder inference and any prepared queries, for example when using 'Database.PostgreSQL.Typed.Protocol.pgSimpleQuery' or 'Database.PostgreSQL.Typed.Protocol.pgSimpleQueries_'.
-- Unlike most other TH functions, this does not require any database connection.
pgSubstituteLiterals :: String -> TH.ExpQ
pgSubstituteLiterals sql = TH.AppE (TH.VarE 'BSL.fromChunks) . TH.ListE <$> mapM sst (sqlTokens sql) where
  sst (SQLExpr e) = do
    v <- either (fail . (++) ("Failed to parse expression {" ++ e ++ "}: ")) return $ parseExp e
    return $ TH.VarE 'pgSafeLiteral `TH.AppE` v
  sst t = return $ TH.VarE 'fromString `TH.AppE` TH.LitE (TH.StringL $ show t)
