{-# LANGUAGE CPP, TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif
-- |
-- Module: Database.PostgreSQL.Typed.Enum
-- Copyright: 2015 Dylan Simon
-- 
-- Support for PostgreSQL enums.

module Database.PostgreSQL.Typed.Enum
  ( PGEnum(..)
  , dataPGEnum
  ) where

import           Control.Arrow ((&&&))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.Ix (Ix)
import           Data.Maybe (fromJust, fromMaybe)
import           Data.Tuple (swap)
import           Data.Typeable (Typeable)
import qualified Language.Haskell.TH as TH

import Database.PostgreSQL.Typed.Types
import Database.PostgreSQL.Typed.Dynamic
import Database.PostgreSQL.Typed.Protocol
import Database.PostgreSQL.Typed.TypeCache
import Database.PostgreSQL.Typed.TH

-- |A type based on a PostgreSQL enum. Automatically instantiated by 'dataPGEnum'.
class (Eq a, Ord a, Enum a, Bounded a, PGRep a) => PGEnum a where
  {-# MINIMAL pgEnumName | pgEnumValues #-}
  -- |The database name of a value.
  pgEnumName :: a -> PGName
  pgEnumName a = fromJust $ lookup a pgEnumValues
  -- |Lookup a value matching the given database name.
  pgEnumValue :: PGName -> Maybe a
  pgEnumValue n = lookup n $ map swap pgEnumValues
  -- |List of all the values in the enum along with their database names.
  pgEnumValues :: [(a, PGName)]
  pgEnumValues = map (id &&& pgEnumName) $ enumFromTo minBound maxBound

-- |Create a new enum type corresponding to the given PostgreSQL enum type.
-- For example, if you have @CREATE TYPE foo AS ENUM (\'abc\', \'DEF\')@, then
-- @dataPGEnum \"Foo\" \"foo\" (\"Foo_\"++)@ will be equivalent to:
-- 
-- > data Foo = Foo_abc | Foo_DEF deriving (Eq, Ord, Enum, Bounded, Typeable)
-- > instance PGType "foo" where PGVal "foo" = Foo
-- > instance PGParameter "foo" Foo where ...
-- > instance PGColumn "foo" Foo where ...
-- > instance PGRep Foo where PGRepType = "foo"
-- > instance PGEnum Foo where pgEnumValues = [(Foo_abc, "abc"), (Foo_DEF, "DEF")]
--
-- Requires language extensions: TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, DataKinds, TypeFamilies
dataPGEnum :: String -- ^ Haskell type to create
  -> PGName -- ^ PostgreSQL enum type name
  -> (String -> String) -- ^ How to generate constructor names from enum values, e.g. @(\"Type_\"++)@ (input is 'pgNameString')
  -> TH.DecsQ
dataPGEnum typs pgenum valnf = do
  (pgid, vals) <- TH.runIO $ withTPGTypeConnection $ \tpg -> do
    vals <- map (\([eo, v]) -> (pgDecodeRep eo, pgDecodeRep v)) . snd
      <$> pgSimpleQuery (pgConnection tpg) (BSL.fromChunks
        [ "SELECT enumtypid, enumlabel"
        ,  " FROM pg_catalog.pg_enum"
        , " WHERE enumtypid = ", pgLiteralRep pgenum, "::regtype"
        , " ORDER BY enumsortorder"
        ])
    case vals of
      [] -> fail $ "dataPGEnum " ++ typs ++ " = " ++ show pgenum ++ ": no values found"
      (eo, _):_ -> do
        et <- maybe (fail $ "dataPGEnum " ++ typs ++ " = " ++ show pgenum ++ ": enum type not found (you may need to use reloadTPGTypes or adjust search_path)") return
          =<< lookupPGType tpg eo
        return (et, map snd vals)
  let valn = map (TH.mkName . valnf . pgNameString &&& map (TH.IntegerL . fromIntegral) . pgNameBytes) vals
      typl = TH.LitT (TH.StrTyLit $ pgNameString pgid)
  dv <- TH.newName "x"
  return $
    [ TH.DataD [] typn []
#if MIN_VERSION_template_haskell(2,11,0)
      Nothing
#endif
      (map (\(n, _) -> TH.NormalC n []) valn) $
#if MIN_VERSION_template_haskell(2,11,0)
#if MIN_VERSION_template_haskell(2,12,0)
      return $ TH.DerivClause Nothing $
#endif
      map TH.ConT
#endif
      [''Eq, ''Ord, ''Enum, ''Ix, ''Bounded, ''Typeable]
    , instanceD [] (TH.ConT ''PGType `TH.AppT` typl)
      [ tySynInstD ''PGVal typl typt
      ]
    , instanceD [] (TH.ConT ''PGParameter `TH.AppT` typl `TH.AppT` typt)
      [ TH.FunD 'pgEncode [TH.Clause [TH.WildP, TH.VarP dv]
        (TH.NormalB $ TH.VarE 'pgNameBS `TH.AppE` (TH.VarE 'pgEnumName `TH.AppE` TH.VarE dv))
        []]
      ]
    , instanceD [] (TH.ConT ''PGColumn `TH.AppT` typl `TH.AppT` typt)
      [ TH.FunD 'pgDecode [TH.Clause [TH.WildP, TH.VarP dv]
        (TH.NormalB $ TH.VarE 'fromMaybe
          `TH.AppE` (TH.AppE (TH.VarE 'error) $
            TH.InfixE (Just $ TH.LitE (TH.StringL ("pgEnumValue " ++ show pgid ++ ": "))) (TH.VarE '(++)) (Just $ TH.VarE 'BSC.unpack `TH.AppE` TH.VarE dv))
          `TH.AppE` (TH.VarE 'pgEnumValue `TH.AppE` (TH.ConE 'PGName
            `TH.AppE` (TH.VarE 'BS.unpack `TH.AppE` TH.VarE dv))))
        []]
      ]
    , instanceD [] (TH.ConT ''PGRep `TH.AppT` typt)
      [ tySynInstD ''PGRepType typt typl
      ]
    , instanceD [] (TH.ConT ''PGEnum `TH.AppT` typt)
      [ TH.FunD 'pgEnumName $ map (\(n, l) -> TH.Clause [conP n []]
        (TH.NormalB $ namelit l)
        []) valn
      , TH.FunD 'pgEnumValue $ map (\(n, l) ->
          TH.Clause [conP 'PGName [TH.ListP (map TH.LitP l)]]
            (TH.NormalB $ TH.ConE 'Just `TH.AppE` TH.ConE n)
            []) valn
          ++ [TH.Clause [TH.WildP] (TH.NormalB $ TH.ConE 'Nothing) []]
      , TH.FunD 'pgEnumValues [TH.Clause []
        (TH.NormalB $ TH.ListE $ map (\(n, l) ->
          TH.ConE '(,) `TH.AppE` TH.ConE n `TH.AppE` namelit l) valn)
        []]
      ]
    , TH.PragmaD $ TH.AnnP (TH.TypeAnnotation typn) $ namelit $ map (TH.IntegerL . fromIntegral) $ pgNameBytes pgid
    ]
    ++ map (\(n, l) ->
      TH.PragmaD $ TH.AnnP (TH.ValueAnnotation n) $ namelit l) valn
  where
  typn = TH.mkName typs
  typt = TH.ConT typn
  instanceD = TH.InstanceD
#if MIN_VERSION_template_haskell(2,11,0)
      Nothing
#endif
  tySynInstD c l t = TH.TySynInstD
#if MIN_VERSION_template_haskell(2,15,0)
    $ TH.TySynEqn Nothing (TH.AppT (TH.ConT c) l)
#else
    c $ TH.TySynEqn [l]
#endif
    t
  namelit l = TH.ConE 'PGName `TH.AppE` TH.ListE (map TH.LitE l)
  conP n p = TH.ConP n
#if MIN_VERSION_template_haskell(2,18,0)
    []
#endif
    p
