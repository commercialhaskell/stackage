{-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances, DataKinds, OverloadedStrings, TypeFamilies #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif
-- |
-- Module: Database.PostgreSQL.Typed.Array
-- Copyright: 2015 Dylan Simon
-- 
-- Representaion of PostgreSQL's array type.
-- Currently this only supports one-dimensional arrays.
-- PostgreSQL arrays in theory can dynamically be any (rectangular) shape.

module Database.PostgreSQL.Typed.Array where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (*>), (<*))
#endif
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import Data.Char (toLower)
import Data.List (intersperse)
import Data.Monoid ((<>))
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat)
#endif
import GHC.TypeLits (Symbol)

import Database.PostgreSQL.Typed.Types

-- |The cannonical representation of a PostgreSQL array of any type, which may always contain NULLs.
-- Currenly only one-dimetional arrays are supported, although in PostgreSQL, any array may be of any dimentionality.
type PGArray a = [Maybe a]

-- |Class indicating that the first PostgreSQL type is an array of the second.
-- This implies 'PGParameter' and 'PGColumn' instances that will work for any type using comma as a delimiter (i.e., anything but @box@).
-- This will only work with 1-dimensional arrays.
class (PGType t, PGType (PGElemType t)) => PGArrayType t where
  type PGElemType t :: Symbol
  pgArrayElementType :: PGTypeID t -> PGTypeID (PGElemType t)
  pgArrayElementType PGTypeProxy = PGTypeProxy
  -- |The character used as a delimeter.  The default @,@ is correct for all standard types (except @box@).
  pgArrayDelim :: PGTypeID t -> Char
  pgArrayDelim _ = ','

instance
#if __GLASGOW_HASKELL__ >= 710
    {-# OVERLAPPING #-}
#endif
    (PGArrayType t, PGParameter (PGElemType t) a) => PGParameter t (PGArray a) where
  pgEncode ta l = buildPGValue $ BSB.char7 '{' <> mconcat (intersperse (BSB.char7 $ pgArrayDelim ta) $ map el l) <> BSB.char7 '}' where
    el Nothing = BSB.string7 "null"
    el (Just e) = pgDQuoteFrom (pgArrayDelim ta : "{}") $ pgEncode (pgArrayElementType ta) e
#if __GLASGOW_HASKELL__ >= 710
-- |Allow entirely non-null arrays as parameter inputs only.
-- (Only supported on ghc >= 7.10 due to instance overlap.)
instance {-# OVERLAPPABLE #-} (PGArrayType t, PGParameter (PGElemType t) a) => PGParameter t [a] where
  pgEncode ta = pgEncode ta . map Just
#endif
instance (PGArrayType t, PGColumn (PGElemType t) a) => PGColumn t (PGArray a) where
  pgDecode ta a = either (error . ("pgDecode array (" ++) . (++ ("): " ++ BSC.unpack a))) id $ P.parseOnly pa a where
    pa = P.char '{' *> P.sepBy (P.skipSpace *> el <* P.skipSpace) (P.char (pgArrayDelim ta)) <* P.char '}' <* P.endOfInput
    el = fmap (pgDecode (pgArrayElementType ta)) <$>
      parsePGDQuote False (pgArrayDelim ta : "{}") (("null" ==) . BSC.map toLower)

-- Just a dump of pg_type:
instance PGType "boolean" => PGType "boolean[]" where
  type PGVal "boolean[]" = PGArray (PGVal "boolean")
instance PGType "boolean" => PGArrayType "boolean[]" where
  type PGElemType "boolean[]" = "boolean"
instance PGType "bytea" => PGType "bytea[]" where
  type PGVal "bytea[]" = PGArray (PGVal "bytea")
instance PGType "bytea" => PGArrayType "bytea[]" where
  type PGElemType "bytea[]" = "bytea"
instance PGType "\"char\"" => PGType "\"char\"[]" where
  type PGVal "\"char\"[]" = PGArray (PGVal "\"char\"")
instance PGType "\"char\"" => PGArrayType "\"char\"[]" where
  type PGElemType "\"char\"[]" = "\"char\""
instance PGType "name" => PGType "name[]" where
  type PGVal "name[]" = PGArray (PGVal "name")
instance PGType "name" => PGArrayType "name[]" where
  type PGElemType "name[]" = "name"
instance PGType "bigint" => PGType "bigint[]" where
  type PGVal "bigint[]" = PGArray (PGVal "bigint")
instance PGType "bigint" => PGArrayType "bigint[]" where
  type PGElemType "bigint[]" = "bigint"
instance PGType "smallint" => PGType "smallint[]" where
  type PGVal "smallint[]" = PGArray (PGVal "smallint")
instance PGType "smallint" => PGArrayType "smallint[]" where
  type PGElemType "smallint[]" = "smallint"
instance PGType "int2vector" => PGType "int2vector[]" where
  type PGVal "int2vector[]" = PGArray (PGVal "int2vector")
instance PGType "int2vector" => PGArrayType "int2vector[]" where
  type PGElemType "int2vector[]" = "int2vector"
instance PGType "integer" => PGType "integer[]" where
  type PGVal "integer[]" = PGArray (PGVal "integer")
instance PGType "integer" => PGArrayType "integer[]" where
  type PGElemType "integer[]" = "integer"
instance PGType "regproc" => PGType "regproc[]" where
  type PGVal "regproc[]" = PGArray (PGVal "regproc")
instance PGType "regproc" => PGArrayType "regproc[]" where
  type PGElemType "regproc[]" = "regproc"
instance PGType "text" => PGType "text[]" where
  type PGVal "text[]" = PGArray (PGVal "text")
instance PGType "text" => PGArrayType "text[]" where
  type PGElemType "text[]" = "text"
instance PGType "oid" => PGType "oid[]" where
  type PGVal "oid[]" = PGArray (PGVal "oid")
instance PGType "oid" => PGArrayType "oid[]" where
  type PGElemType "oid[]" = "oid"
instance PGType "tid" => PGType "tid[]" where
  type PGVal "tid[]" = PGArray (PGVal "tid")
instance PGType "tid" => PGArrayType "tid[]" where
  type PGElemType "tid[]" = "tid"
instance PGType "xid" => PGType "xid[]" where
  type PGVal "xid[]" = PGArray (PGVal "xid")
instance PGType "xid" => PGArrayType "xid[]" where
  type PGElemType "xid[]" = "xid"
instance PGType "cid" => PGType "cid[]" where
  type PGVal "cid[]" = PGArray (PGVal "cid")
instance PGType "cid" => PGArrayType "cid[]" where
  type PGElemType "cid[]" = "cid"
instance PGType "oidvector" => PGType "oidvector[]" where
  type PGVal "oidvector[]" = PGArray (PGVal "oidvector")
instance PGType "oidvector" => PGArrayType "oidvector[]" where
  type PGElemType "oidvector[]" = "oidvector"
instance PGType "json" => PGType "json[]" where
  type PGVal "json[]" = PGArray (PGVal "json")
instance PGType "json" => PGArrayType "json[]" where
  type PGElemType "json[]" = "json"
instance PGType "xml" => PGType "xml[]" where
  type PGVal "xml[]" = PGArray (PGVal "xml")
instance PGType "xml" => PGArrayType "xml[]" where
  type PGElemType "xml[]" = "xml"
instance PGType "point" => PGType "point[]" where
  type PGVal "point[]" = PGArray (PGVal "point")
instance PGType "point" => PGArrayType "point[]" where
  type PGElemType "point[]" = "point"
instance PGType "lseg" => PGType "lseg[]" where
  type PGVal "lseg[]" = PGArray (PGVal "lseg")
instance PGType "lseg" => PGArrayType "lseg[]" where
  type PGElemType "lseg[]" = "lseg"
instance PGType "path" => PGType "path[]" where
  type PGVal "path[]" = PGArray (PGVal "path")
instance PGType "path" => PGArrayType "path[]" where
  type PGElemType "path[]" = "path"
instance PGType "box" => PGType "box[]" where
  type PGVal "box[]" = PGArray (PGVal "box")
instance PGType "box" => PGArrayType "box[]" where
  type PGElemType "box[]" = "box"
  pgArrayDelim _ = ';'
instance PGType "polygon" => PGType "polygon[]" where
  type PGVal "polygon[]" = PGArray (PGVal "polygon")
instance PGType "polygon" => PGArrayType "polygon[]" where
  type PGElemType "polygon[]" = "polygon"
instance PGType "line" => PGType "line[]" where
  type PGVal "line[]" = PGArray (PGVal "line")
instance PGType "line" => PGArrayType "line[]" where
  type PGElemType "line[]" = "line"
instance PGType "cidr" => PGType "cidr[]" where
  type PGVal "cidr[]" = PGArray (PGVal "cidr")
instance PGType "cidr" => PGArrayType "cidr[]" where
  type PGElemType "cidr[]" = "cidr"
instance PGType "real" => PGType "real[]" where
  type PGVal "real[]" = PGArray (PGVal "real")
instance PGType "real" => PGArrayType "real[]" where
  type PGElemType "real[]" = "real"
instance PGType "double precision" => PGType "double precision[]" where
  type PGVal "double precision[]" = PGArray (PGVal "double precision")
instance PGType "double precision" => PGArrayType "double precision[]" where
  type PGElemType "double precision[]" = "double precision"
instance PGType "abstime" => PGType "abstime[]" where
  type PGVal "abstime[]" = PGArray (PGVal "abstime")
instance PGType "abstime" => PGArrayType "abstime[]" where
  type PGElemType "abstime[]" = "abstime"
instance PGType "reltime" => PGType "reltime[]" where
  type PGVal "reltime[]" = PGArray (PGVal "reltime")
instance PGType "reltime" => PGArrayType "reltime[]" where
  type PGElemType "reltime[]" = "reltime"
instance PGType "tinterval" => PGType "tinterval[]" where
  type PGVal "tinterval[]" = PGArray (PGVal "tinterval")
instance PGType "tinterval" => PGArrayType "tinterval[]" where
  type PGElemType "tinterval[]" = "tinterval"
instance PGType "circle" => PGType "circle[]" where
  type PGVal "circle[]" = PGArray (PGVal "circle")
instance PGType "circle" => PGArrayType "circle[]" where
  type PGElemType "circle[]" = "circle"
instance PGType "money" => PGType "money[]" where
  type PGVal "money[]" = PGArray (PGVal "money")
instance PGType "money" => PGArrayType "money[]" where
  type PGElemType "money[]" = "money"
instance PGType "macaddr" => PGType "macaddr[]" where
  type PGVal "macaddr[]" = PGArray (PGVal "macaddr")
instance PGType "macaddr" => PGArrayType "macaddr[]" where
  type PGElemType "macaddr[]" = "macaddr"
instance PGType "inet" => PGType "inet[]" where
  type PGVal "inet[]" = PGArray (PGVal "inet")
instance PGType "inet" => PGArrayType "inet[]" where
  type PGElemType "inet[]" = "inet"
instance PGType "aclitem" => PGType "aclitem[]" where
  type PGVal "aclitem[]" = PGArray (PGVal "aclitem")
instance PGType "aclitem" => PGArrayType "aclitem[]" where
  type PGElemType "aclitem[]" = "aclitem"
instance PGType "bpchar" => PGType "bpchar[]" where
  type PGVal "bpchar[]" = PGArray (PGVal "bpchar")
instance PGType "bpchar" => PGArrayType "bpchar[]" where
  type PGElemType "bpchar[]" = "bpchar"
instance PGType "character varying" => PGType "character varying[]" where
  type PGVal "character varying[]" = PGArray (PGVal "character varying")
instance PGType "character varying" => PGArrayType "character varying[]" where
  type PGElemType "character varying[]" = "character varying"
instance PGType "date" => PGType "date[]" where
  type PGVal "date[]" = PGArray (PGVal "date")
instance PGType "date" => PGArrayType "date[]" where
  type PGElemType "date[]" = "date"
instance PGType "time without time zone" => PGType "time without time zone[]" where
  type PGVal "time without time zone[]" = PGArray (PGVal "time without time zone")
instance PGType "time without time zone" => PGArrayType "time without time zone[]" where
  type PGElemType "time without time zone[]" = "time without time zone"
instance PGType "timestamp without time zone" => PGType "timestamp without time zone[]" where
  type PGVal "timestamp without time zone[]" = PGArray (PGVal "timestamp without time zone")
instance PGType "timestamp without time zone" => PGArrayType "timestamp without time zone[]" where
  type PGElemType "timestamp without time zone[]" = "timestamp without time zone"
instance PGType "timestamp with time zone" => PGType "timestamp with time zone[]" where
  type PGVal "timestamp with time zone[]" = PGArray (PGVal "timestamp with time zone")
instance PGType "timestamp with time zone" => PGArrayType "timestamp with time zone[]" where
  type PGElemType "timestamp with time zone[]" = "timestamp with time zone"
instance PGType "interval" => PGType "interval[]" where
  type PGVal "interval[]" = PGArray (PGVal "interval")
instance PGType "interval" => PGArrayType "interval[]" where
  type PGElemType "interval[]" = "interval"
instance PGType "time with time zone" => PGType "time with time zone[]" where
  type PGVal "time with time zone[]" = PGArray (PGVal "time with time zone")
instance PGType "time with time zone" => PGArrayType "time with time zone[]" where
  type PGElemType "time with time zone[]" = "time with time zone"
instance PGType "bit" => PGType "bit[]" where
  type PGVal "bit[]" = PGArray (PGVal "bit")
instance PGType "bit" => PGArrayType "bit[]" where
  type PGElemType "bit[]" = "bit"
instance PGType "varbit" => PGType "varbit[]" where
  type PGVal "varbit[]" = PGArray (PGVal "varbit")
instance PGType "varbit" => PGArrayType "varbit[]" where
  type PGElemType "varbit[]" = "varbit"
instance PGType "numeric" => PGType "numeric[]" where
  type PGVal "numeric[]" = PGArray (PGVal "numeric")
instance PGType "numeric" => PGArrayType "numeric[]" where
  type PGElemType "numeric[]" = "numeric"
instance PGType "refcursor" => PGType "refcursor[]" where
  type PGVal "refcursor[]" = PGArray (PGVal "refcursor")
instance PGType "refcursor" => PGArrayType "refcursor[]" where
  type PGElemType "refcursor[]" = "refcursor"
instance PGType "regprocedure" => PGType "regprocedure[]" where
  type PGVal "regprocedure[]" = PGArray (PGVal "regprocedure")
instance PGType "regprocedure" => PGArrayType "regprocedure[]" where
  type PGElemType "regprocedure[]" = "regprocedure"
instance PGType "regoper" => PGType "regoper[]" where
  type PGVal "regoper[]" = PGArray (PGVal "regoper")
instance PGType "regoper" => PGArrayType "regoper[]" where
  type PGElemType "regoper[]" = "regoper"
instance PGType "regoperator" => PGType "regoperator[]" where
  type PGVal "regoperator[]" = PGArray (PGVal "regoperator")
instance PGType "regoperator" => PGArrayType "regoperator[]" where
  type PGElemType "regoperator[]" = "regoperator"
instance PGType "regclass" => PGType "regclass[]" where
  type PGVal "regclass[]" = PGArray (PGVal "regclass")
instance PGType "regclass" => PGArrayType "regclass[]" where
  type PGElemType "regclass[]" = "regclass"
instance PGType "regtype" => PGType "regtype[]" where
  type PGVal "regtype[]" = PGArray (PGVal "regtype")
instance PGType "regtype" => PGArrayType "regtype[]" where
  type PGElemType "regtype[]" = "regtype"
instance PGType "record" => PGType "record[]" where
  type PGVal "record[]" = PGArray (PGVal "record")
instance PGType "record" => PGArrayType "record[]" where
  type PGElemType "record[]" = "record"
instance PGType "cstring" => PGType "cstring[]" where
  type PGVal "cstring[]" = PGArray (PGVal "cstring")
instance PGType "cstring" => PGArrayType "cstring[]" where
  type PGElemType "cstring[]" = "cstring"
instance PGType "uuid" => PGType "uuid[]" where
  type PGVal "uuid[]" = PGArray (PGVal "uuid")
instance PGType "uuid" => PGArrayType "uuid[]" where
  type PGElemType "uuid[]" = "uuid"
instance PGType "txid_snapshot" => PGType "txid_snapshot[]" where
  type PGVal "txid_snapshot[]" = PGArray (PGVal "txid_snapshot")
instance PGType "txid_snapshot" => PGArrayType "txid_snapshot[]" where
  type PGElemType "txid_snapshot[]" = "txid_snapshot"
instance PGType "tsvector" => PGType "tsvector[]" where
  type PGVal "tsvector[]" = PGArray (PGVal "tsvector")
instance PGType "tsvector" => PGArrayType "tsvector[]" where
  type PGElemType "tsvector[]" = "tsvector"
instance PGType "tsquery" => PGType "tsquery[]" where
  type PGVal "tsquery[]" = PGArray (PGVal "tsquery")
instance PGType "tsquery" => PGArrayType "tsquery[]" where
  type PGElemType "tsquery[]" = "tsquery"
instance PGType "gtsvector" => PGType "gtsvector[]" where
  type PGVal "gtsvector[]" = PGArray (PGVal "gtsvector")
instance PGType "gtsvector" => PGArrayType "gtsvector[]" where
  type PGElemType "gtsvector[]" = "gtsvector"
instance PGType "regconfig" => PGType "regconfig[]" where
  type PGVal "regconfig[]" = PGArray (PGVal "regconfig")
instance PGType "regconfig" => PGArrayType "regconfig[]" where
  type PGElemType "regconfig[]" = "regconfig"
instance PGType "regdictionary" => PGType "regdictionary[]" where
  type PGVal "regdictionary[]" = PGArray (PGVal "regdictionary")
instance PGType "regdictionary" => PGArrayType "regdictionary[]" where
  type PGElemType "regdictionary[]" = "regdictionary"
instance PGType "int4range" => PGType "int4range[]" where
  type PGVal "int4range[]" = PGArray (PGVal "int4range")
instance PGType "int4range" => PGArrayType "int4range[]" where
  type PGElemType "int4range[]" = "int4range"
instance PGType "numrange" => PGType "numrange[]" where
  type PGVal "numrange[]" = PGArray (PGVal "numrange")
instance PGType "numrange" => PGArrayType "numrange[]" where
  type PGElemType "numrange[]" = "numrange"
instance PGType "tsrange" => PGType "tsrange[]" where
  type PGVal "tsrange[]" = PGArray (PGVal "tsrange")
instance PGType "tsrange" => PGArrayType "tsrange[]" where
  type PGElemType "tsrange[]" = "tsrange"
instance PGType "tstzrange" => PGType "tstzrange[]" where
  type PGVal "tstzrange[]" = PGArray (PGVal "tstzrange")
instance PGType "tstzrange" => PGArrayType "tstzrange[]" where
  type PGElemType "tstzrange[]" = "tstzrange"
instance PGType "daterange" => PGType "daterange[]" where
  type PGVal "daterange[]" = PGArray (PGVal "daterange")
instance PGType "daterange" => PGArrayType "daterange[]" where
  type PGElemType "daterange[]" = "daterange"
instance PGType "int8range" => PGType "int8range[]" where
  type PGVal "int8range[]" = PGArray (PGVal "int8range")
instance PGType "int8range" => PGArrayType "int8range[]" where
  type PGElemType "int8range[]" = "int8range"

