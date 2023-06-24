{-# LANGUAGE CPP, PatternGuards, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, GADTs, DataKinds, TemplateHaskell #-}
module Database.PostgreSQL.Typed.Query
  ( PGQuery(..)
  , PGSimpleQuery
  , PGPreparedQuery
  , rawPGSimpleQuery
  , rawPGPreparedQuery
  , QueryFlags(..)
  , simpleQueryFlags
  , parseQueryFlags
  , makePGQuery
  , pgSQL
  , pgExecute
  , pgQuery
  , pgLazyQuery
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Arrow ((***), first, second)
import Control.Exception (try)
import Control.Monad (void, when, mapAndUnzipM)
import Data.Array (listArray, (!), inRange)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Data.ByteString.UTF8 as BSU
import Data.Char (isSpace, isAlphaNum)
import qualified Data.Foldable as Fold
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe, isNothing)
import Data.String (IsString(..))
import Data.Word (Word32)
import Language.Haskell.Meta.Parse (parseExp)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Database.PostgreSQL.Typed.Types
import Database.PostgreSQL.Typed.Dynamic
import Database.PostgreSQL.Typed.Protocol
import Database.PostgreSQL.Typed.TH
import Database.PostgreSQL.Typed.SQLToken

class PGQuery q a | q -> a where
  -- |Execute a query and return the number of rows affected (or -1 if not known) and a list of results.
  pgRunQuery :: PGConnection -> q -> IO (Int, [a])
  -- |Change the raw SQL query stored within this query.
  -- This is unsafe because the query has already been type-checked, so any change must not change the number or type of results or placeholders (so adding additional static WHERE or ORDER BY clauses is generally safe).
  -- This is useful in cases where you need to construct some part of the query dynamically, but still want to infer the result types.
  -- If you want to add dynamic values to the query, it's best to use 'Database.PostgreSQL.Typed.Dynamic.pgSafeLiteral'.
  -- For example:
  --
  -- > [pgSQL|SELECT a FROM t|] `unsafeModifyQuery` (<> (" WHERE a = " <> pgSafeLiteral x))
  unsafeModifyQuery :: q -> (BS.ByteString -> BS.ByteString) -> q
  getQueryString :: PGTypeEnv -> q -> BS.ByteString
class PGQuery q PGValues => PGRawQuery q

-- |Execute a query that does not return results.
-- Return the number of rows affected (or -1 if not known).
pgExecute :: PGQuery q () => PGConnection -> q -> IO Int
pgExecute c q = fst <$> pgRunQuery c q

-- |Run a query and return a list of row results.
pgQuery :: PGQuery q a => PGConnection -> q -> IO [a]
pgQuery c q = snd <$> pgRunQuery c q

instance PGQuery BS.ByteString PGValues where
  pgRunQuery c sql = pgSimpleQuery c (BSL.fromStrict sql)
  unsafeModifyQuery q f = f q
  getQueryString _ = id

newtype SimpleQuery = SimpleQuery BS.ByteString
  deriving (Show)
instance PGQuery SimpleQuery PGValues where
  pgRunQuery c (SimpleQuery sql) = pgSimpleQuery c (BSL.fromStrict sql)
  unsafeModifyQuery (SimpleQuery sql) f = SimpleQuery $ f sql
  getQueryString _ (SimpleQuery q) = q
instance PGRawQuery SimpleQuery

data PreparedQuery = PreparedQuery BS.ByteString [OID] PGValues [Bool]
  deriving (Show)
instance PGQuery PreparedQuery PGValues where
  pgRunQuery c (PreparedQuery sql types bind bc) = pgPreparedQuery c sql types bind bc
  unsafeModifyQuery (PreparedQuery sql types bind bc) f = PreparedQuery (f sql) types bind bc
  getQueryString _ (PreparedQuery q _ _ _) = q
instance PGRawQuery PreparedQuery


data QueryParser q a = QueryParser (PGTypeEnv -> q) (PGTypeEnv -> PGValues -> a)
instance PGRawQuery q => PGQuery (QueryParser q a) a where
  pgRunQuery c (QueryParser q p) = second (fmap $ p e) <$> pgRunQuery c (q e) where e = pgTypeEnv c
  unsafeModifyQuery (QueryParser q p) f = QueryParser (\e -> unsafeModifyQuery (q e) f) p
  getQueryString e (QueryParser q _) = getQueryString e $ q e

instance Functor (QueryParser q) where
  fmap f (QueryParser q p) = QueryParser q (\e -> f . p e)

instance Show q => Show (QueryParser q a) where
  showsPrec p (QueryParser q _) = showParen (p > 10) $
    showString "QueryParser " . showsPrec 11 (q unknownPGTypeEnv)

rawParser :: q -> QueryParser q PGValues
rawParser q = QueryParser (const q) (const id)

-- |A simple one-shot query that simply substitutes literal representations of parameters for placeholders.
type PGSimpleQuery = QueryParser SimpleQuery
-- |A prepared query that automatically is prepared in the database the first time it is run and bound with new parameters each subsequent time.
type PGPreparedQuery = QueryParser PreparedQuery

-- |Make a simple query directly from a query string, with no type inference
rawPGSimpleQuery :: BS.ByteString -> PGSimpleQuery PGValues
rawPGSimpleQuery = rawParser . SimpleQuery

instance IsString (PGSimpleQuery PGValues) where
  fromString = rawPGSimpleQuery . fromString
instance IsString (PGSimpleQuery ()) where
  fromString = void . rawPGSimpleQuery . fromString

-- |Make a prepared query directly from a query string and bind parameters, with no type inference
rawPGPreparedQuery :: BS.ByteString -> PGValues -> PGPreparedQuery PGValues
rawPGPreparedQuery sql bind = rawParser $ PreparedQuery sql [] bind []

-- |Run a prepared query in lazy mode, where only chunk size rows are requested at a time.
-- If you eventually retrieve all the rows this way, it will be far less efficient than using @pgQuery@, since every chunk requires an additional round-trip.
-- Although you may safely stop consuming rows early, currently you may not interleave any other database operation while reading rows.  (This limitation could theoretically be lifted if required.)
pgLazyQuery :: PGConnection -> PGPreparedQuery a -> Word32 -- ^ Chunk size (1 is common, 0 is all-or-nothing)
  -> IO [a]
pgLazyQuery c (QueryParser q p) count =
  fmap (p e) <$> pgPreparedLazyQuery c sql types bind bc count where
  e = pgTypeEnv c
  PreparedQuery sql types bind bc = q e

-- |Given a SQL statement with placeholders of the form @${expr}@, return a (hopefully) valid SQL statement with @$N@ placeholders and the list of expressions.
-- This does its best to understand SQL syntax, so placeholders are only interpreted in places postgres would understand them (i.e., not in quoted strings).  Since this is not valid SQL otherwise, there is never reason to escape a literal @${@.
-- You can use @$N@ placeholders in the query otherwise to refer to the N-th index placeholder expression.
sqlPlaceholders :: String -> (String, [String])
sqlPlaceholders = sst (1 :: Int) . sqlTokens where
  sst n (SQLExpr e : l) = (('$':show n) ++) *** (e :) $ sst (succ n) l
  sst n (t : l) = first (show t ++) $ sst n l
  sst _ [] = ("", [])

-- |Given a SQL statement with placeholders of the form @$N@ and a list of TH 'ByteString' expressions, return a new 'ByteString' expression that substitutes the expressions for the placeholders.
sqlSubstitute :: String -> [TH.Exp] -> TH.Exp
sqlSubstitute sql exprl = TH.AppE (TH.VarE 'BS.concat) $ TH.ListE $ map sst $ sqlTokens sql where
  bnds = (1, length exprl)
  exprs = listArray bnds exprl
  expr n
    | inRange bnds n = exprs ! n
    | otherwise = error $ "SQL placeholder '$" ++ show n ++ "' out of range (not recognized by PostgreSQL)"
  sst (SQLParam n) = expr n
  sst t = TH.VarE 'BSU.fromString `TH.AppE` TH.LitE (TH.StringL $ show t)

splitCommas :: String -> [String]
splitCommas = spl where
  spl [] = []
  spl [c] = [[c]]
  spl (',':s) = "":spl s
  spl (c:s) = (c:h):t where h:t = spl s

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- |Flags affecting how and what type of query to build with 'makePGQuery'.
data QueryFlags = QueryFlags
  { flagQuery :: Bool -- ^ Create a query -- otherwise just call 'pgSubstituteLiterals' to create a string (SQL fragment).
  , flagNullable :: Maybe Bool -- ^ Disable nullability inference, treating all values as nullable (if 'True') or not (if 'False').
  , flagPrepare :: Maybe [String] -- ^ Prepare and re-use query, binding parameters of the given types (inferring the rest, like PREPARE).
  }

-- |'QueryFlags' for a default (simple) query.
simpleQueryFlags :: QueryFlags
simpleQueryFlags = QueryFlags True Nothing Nothing

newName :: Char -> BS.ByteString -> TH.Q TH.Name
newName pre = TH.newName . ('_':) . (pre:) . filter (\c -> isAlphaNum c || c == '_') . BSC.unpack

-- |Construct a 'PGQuery' from a SQL string.
-- This is the underlying template function for 'pgSQL' which you can use in largely the same way when you want to construct query strings from other variables.
-- For example:
--
-- > selectQuery = "SELECT * FROM"
-- > selectFoo = $(makePGQuery simpleQueryFlags (selectQuery ++ " foo"))
--
-- The only caveat is that variables or functions like @selectQuery@ need to be defined in a different module (due to TH stage restrictions).
makePGQuery :: QueryFlags -> String -> TH.ExpQ
makePGQuery QueryFlags{ flagQuery = False } sqle = pgSubstituteLiterals sqle
makePGQuery QueryFlags{ flagNullable = nulls, flagPrepare = prep } sqle = do
  (pt, rt) <- TH.runIO $ tpgDescribe (BSU.fromString sqlp) (fromMaybe [] prep) (isNothing nulls)
  when (length pt < length exprs) $ fail "Not all expression placeholders were recognized by PostgreSQL"

  e <- TH.newName "_tenv"
  l <- TH.newName "l"
  (vars, vals) <- mapAndUnzipM (\t -> do
    v <- newName 'p' $ tpgValueName t
    return 
      ( TH.VarP v
      , tpgTypeEncoder (isNothing prep) t e `TH.AppE` TH.VarE v
      )) pt
  (pats, conv, bins) <- unzip3 <$> mapM (\t -> do
    v <- newName 'c' $ tpgValueName t
    return 
      ( TH.VarP v
      , tpgTypeDecoder (Fold.and nulls) t e `TH.AppE` TH.VarE v
      , tpgTypeBinary t e
      )) rt
  foldl TH.AppE (TH.LamE vars $ TH.ConE 'QueryParser
    `TH.AppE` TH.LamE [TH.VarP e] (maybe
      (TH.ConE 'SimpleQuery
        `TH.AppE` sqlSubstitute sqlp vals)
      (\p -> TH.ConE 'PreparedQuery
        `TH.AppE` (TH.VarE 'BSU.fromString `TH.AppE` TH.LitE (TH.StringL sqlp))
        `TH.AppE` TH.ListE (map (TH.LitE . TH.IntegerL . toInteger . tpgValueTypeOID . snd) $ zip p pt)
        `TH.AppE` TH.ListE vals 
        `TH.AppE` TH.ListE 
#ifdef VERSION_postgresql_binary
          bins
#else
          []
#endif
      )
      prep)
    `TH.AppE` TH.LamE [TH.VarP e, TH.VarP l] (TH.CaseE (TH.VarE l)
      [ TH.Match (TH.ListP pats) (TH.NormalB $ case conv of
          [x] -> x
          _ -> TH.TupE
#if MIN_VERSION_template_haskell(2,16,0)
            $ map Just
#endif
            conv) []
      , TH.Match TH.WildP (TH.NormalB $ TH.VarE 'error `TH.AppE` TH.LitE (TH.StringL "pgSQL: result arity mismatch")) []
      ]))
    <$> mapM parse exprs
  where
  (sqlp, exprs) = sqlPlaceholders sqle
  parse e = either (fail . (++) ("Failed to parse expression {" ++ e ++ "}: ")) return $ parseExp e

-- |Parse flags off the beginning of a query string, returning the flags and the remaining string.
parseQueryFlags :: String -> (QueryFlags, String)
parseQueryFlags = pqf simpleQueryFlags where
  pqf f@QueryFlags{ flagQuery = True, flagPrepare = Nothing } ('#':q) = pqf f{ flagQuery = False } q
  pqf f@QueryFlags{ flagQuery = True, flagNullable = Nothing } ('?':q) = pqf f{ flagNullable = Just True } q
  pqf f@QueryFlags{ flagQuery = True, flagNullable = Nothing } ('!':q) = pqf f{ flagNullable = Just False } q
  pqf f@QueryFlags{ flagQuery = True, flagPrepare = Nothing } ('$':q) = pqf f{ flagPrepare = Just [] } q
  pqf f@QueryFlags{ flagQuery = True, flagPrepare = Just [] } ('(':s) = pqf f{ flagPrepare = Just args } (sql r) where
    args = map trim $ splitCommas arg
    (arg, r) = break (')' ==) s
    sql (')':q) = q
    sql _ = error "pgSQL: unterminated argument list" 
  pqf f q = (f, q)

qqQuery :: String -> TH.ExpQ
qqQuery = uncurry makePGQuery . parseQueryFlags

qqTop :: Bool -> String -> TH.DecsQ
qqTop True ('!':sql) = qqTop False sql
qqTop err sql = do
  r <- TH.runIO $ try $ withTPGConnection $ \c ->
    pgSimpleQuery c (BSLU.fromString sql)
  either ((if err then TH.reportError else TH.reportWarning) . (show :: PGError -> String)) (const $ return ()) r
  return []

-- |A quasi-quoter for PGSQL queries.
--
-- Used in expression context, it may contain any SQL statement @[pgSQL|SELECT ...|]@.
-- The statement may contain PostgreSQL-style placeholders (@$1@, @$2@, ...) or in-line placeholders (@${1+1}@) containing any valid Haskell expression (except @{}@).
-- It will be replaced by a 'PGQuery' object that can be used to perform the SQL statement.
-- If there are more @$N@ placeholders than expressions, it will instead be a function accepting the additional parameters and returning a 'PGQuery'.
-- 
-- Ideally, this mimics postgres' SQL parsing, so that placeholders and expressions will only be expanded when they are in valid positions (i.e., not inside quoted strings).
-- Since @${@ is not valid SQL otherwise, there should be no need to escape it.
--
-- The statement may start with one of more special flags affecting the interpretation:
--
-- [@?@] To disable nullability inference, treating all result values as nullable, thus returning 'Maybe' values regardless of inferred nullability. This makes unexpected NULL errors impossible.
-- [@!@] To disable nullability inference, treating all result values as /not/ nullable, thus only returning 'Maybe' where requested. This is makes unexpected NULL errors more likely.
-- [@$@] To create a 'PGPreparedQuery' (using placeholder parameters) rather than the default 'PGSimpleQuery' (using literal substitution).
-- [@$(type,...)@] To specify specific types for a prepared query (see <http://www.postgresql.org/docs/current/static/sql-prepare.html> for details), rather than inferring parameter types by default.
-- [@#@] Only do literal @${}@ substitution using 'pgSubstituteLiterals' and return a string, not a query.
-- 
-- 'pgSQL' can also be used at the top-level to execute SQL statements at compile-time (without any parameters and ignoring results).
-- Here the query can only be prefixed with @!@ to make errors non-fatal.
--
-- If you want to construct queries out of string variables rather than quasi-quoted strings, you can use the lower-level 'makePGQuery' instead.
pgSQL :: QuasiQuoter
pgSQL = QuasiQuoter
  { quoteExp = qqQuery
  , quoteType = const $ fail "pgSQL not supported in types"
  , quotePat = const $ fail "pgSQL not supported in patterns"
  , quoteDec = qqTop True
  }
