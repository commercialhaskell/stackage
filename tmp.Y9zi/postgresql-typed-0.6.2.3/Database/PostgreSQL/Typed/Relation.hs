{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif
-- |
-- Module: Database.PostgreSQL.Typed.Relation
-- Copyright: 2016 Dylan Simon
-- 
-- Automatically create data types based on tables and other relations.

module Database.PostgreSQL.Typed.Relation
  ( dataPGRelation
  ) where

import qualified Data.ByteString.Lazy as BSL
import           Data.Proxy (Proxy(..))
import qualified Language.Haskell.TH as TH

import           Database.PostgreSQL.Typed.Types
import           Database.PostgreSQL.Typed.Dynamic
import           Database.PostgreSQL.Typed.Protocol
import           Database.PostgreSQL.Typed.TypeCache
import           Database.PostgreSQL.Typed.TH

-- |Data types that are based on database relations.
-- Normally these instances are created using 'dataPGRelation'.
class (PGRep a, PGRecordType (PGRepType a)) => PGRelation a where
  -- |Database name of table/relation (i.e., second argument to 'dataPGRelation').  Normally this is the same as @'pgTypeID' . 'pgTypeOfProxy'@, but this preserves any specified schema qualification.
  pgRelationName :: Proxy a -> PGName
  pgRelationName = pgTypeName . pgTypeOfProxy
  -- |Database names of columns.
  pgColumnNames :: Proxy a -> [PGName]

-- |Create a new data type corresponding to the given PostgreSQL relation.
-- For example, if you have @CREATE TABLE foo (abc integer NOT NULL, def text)@, then
-- @dataPGRelation \"Foo\" \"foo\" (\"foo_\"++)@ will be equivalent to:
-- 
-- > data Foo = Foo{ foo_abc :: PGVal "integer", foo_def :: Maybe (PGVal "text") }
-- > instance PGType "foo" where PGVal "foo" = Foo
-- > instance PGParameter "foo" Foo where ...
-- > instance PGColumn "foo" Foo where ...
-- > instance PGColumn "foo" (Maybe Foo) where ... -- to handle NULL in not null columns
-- > instance PGRep Foo where PGRepType = "foo"
-- > instance PGRecordType "foo"
-- > instance PGRelation Foo where pgColumnNames _ = ["abc", "def"]
-- > uncurryFoo :: (PGVal "integer", Maybe (PGVal "text")) -> Foo
--
-- (Note that @PGVal "integer" = Int32@ and @PGVal "text" = Text@ by default.)
-- This provides instances for marshalling the corresponding composite/record types, e.g., using @SELECT foo.*::foo FROM foo@.
-- If you want any derived instances, you'll need to create them yourself using StandaloneDeriving.
--
-- Requires language extensions: TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, DataKinds, TypeFamilies, PatternGuards
dataPGRelation :: String -- ^ Haskell type and constructor to create
  -> PGName -- ^ PostgreSQL table/relation name
  -> (String -> String) -- ^ How to generate field names from column names, e.g. @("table_"++)@ (input is 'pgNameString')
  -> TH.DecsQ
dataPGRelation typs pgtab colf = do
  (pgid, cold) <- TH.runIO $ withTPGTypeConnection $ \tpg -> do
    cl <- mapM (\[to, cn, ct, cnn] -> do
      let c = pgDecodeRep cn :: PGName
          n = TH.mkName $ colf $ pgNameString c
          o = pgDecodeRep ct :: OID
      t <- maybe (fail $ "dataPGRelation " ++ typs ++ " = " ++ show pgtab ++ ": column '" ++ show c ++ "' has unknown type " ++ show o) return
        =<< lookupPGType tpg o
      return (pgDecodeRep to, (c, n, TH.LitT (TH.StrTyLit $ pgNameString t), not $ pgDecodeRep cnn)))
      . snd =<< pgSimpleQuery (pgConnection tpg) (BSL.fromChunks
        [ "SELECT reltype, attname, atttypid, attnotnull"
        ,  " FROM pg_catalog.pg_attribute"
        ,  " JOIN pg_catalog.pg_class ON attrelid = pg_class.oid"
        , " WHERE attrelid = ", pgLiteralRep pgtab, "::regclass"
        ,   " AND attnum > 0 AND NOT attisdropped"
        , " ORDER BY attnum"
        ])
    case cl of
      [] -> fail $ "dataPGRelation " ++ typs ++ " = " ++ show pgtab ++ ": no columns found"
      (to, _):_ -> do
        tt <- maybe (fail $ "dataPGRelation " ++ typs ++ " = " ++ show pgtab ++ ": table type not found (you may need to use reloadTPGTypes or adjust search_path)") return
          =<< lookupPGType tpg to
        return (tt, map snd cl)
  cols <- mapM (\(c, _, t, nn) -> do
      v <- TH.newName $ pgNameString c
      return (v, t, nn))
    cold
  let typl = TH.LitT (TH.StrTyLit $ pgNameString pgid)
      encfun f = TH.FunD f [TH.Clause [TH.WildP, conP typn (map (\(v, _, _) -> TH.VarP v) cols)]
        (TH.NormalB $ pgcall f rect `TH.AppE`
          (TH.ConE 'PGRecord `TH.AppE` TH.ListE (map (colenc f) cols)))
        [] ]
  dv <- TH.newName "x"
  tv <- TH.newName "t"
  ev <- TH.newName "e"
  return $
    [ TH.DataD
      []
      typn
      []
#if MIN_VERSION_template_haskell(2,11,0)
      Nothing
#endif
      [ TH.RecC typn $ map (\(_, n, t, nn) ->
        ( n
#if MIN_VERSION_template_haskell(2,11,0)
        , TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness
#else
        , TH.NotStrict
#endif
        , (if nn then (TH.ConT ''Maybe `TH.AppT`) else id)
          (TH.ConT ''PGVal `TH.AppT` t)))
        cold
      ]
      []
    , instanceD [] (TH.ConT ''PGType `TH.AppT` typl)
      [ tySynInstD ''PGVal typl typt
      ]
    , instanceD [] (TH.ConT ''PGParameter `TH.AppT` typl `TH.AppT` typt)
      [ encfun 'pgEncode
      , encfun 'pgLiteral
      ]
    , instanceD [] (TH.ConT ''PGColumn `TH.AppT` typl `TH.AppT` typt)
      [ TH.FunD 'pgDecode [TH.Clause [TH.WildP, TH.VarP dv]
        (TH.GuardedB
          [ (TH.PatG [TH.BindS
              (conP 'PGRecord [TH.ListP $ map colpat cols])
              (pgcall 'pgDecode rect `TH.AppE` TH.VarE dv)]
            , foldl (\f -> TH.AppE f . coldec) (TH.ConE typn) cols)
          , (TH.NormalG (TH.ConE 'True)
            , TH.VarE 'error `TH.AppE` TH.LitE (TH.StringL $ "pgDecode " ++ typs ++ ": NULL in not null record column"))
          ])
        [] ]
      ]
#if MIN_VERSION_template_haskell(2,11,0)
    , TH.InstanceD (Just TH.Overlapping) [] (TH.ConT ''PGColumn `TH.AppT` typl `TH.AppT` (TH.ConT ''Maybe `TH.AppT` typt))
      [ TH.FunD 'pgDecode [TH.Clause [TH.WildP, TH.VarP dv]
        (TH.GuardedB
          [ (TH.PatG [TH.BindS
              (conP 'PGRecord [TH.ListP $ map colpat cols])
              (pgcall 'pgDecode rect `TH.AppE` TH.VarE dv)]
            , TH.ConE 'Just `TH.AppE` foldl (\f -> TH.AppE f . coldec) (TH.ConE typn) cols)
          , (TH.NormalG (TH.ConE 'True)
            , TH.ConE 'Nothing)
          ])
        [] ]
      , TH.FunD 'pgDecodeValue
        [ TH.Clause [TH.WildP, TH.WildP, conP 'PGNullValue []]
          (TH.NormalB $ TH.ConE 'Nothing)
          []
        , TH.Clause [TH.WildP, TH.VarP tv, conP 'PGTextValue [TH.VarP dv]]
          (TH.NormalB $ TH.VarE 'pgDecode `TH.AppE` TH.VarE tv `TH.AppE` TH.VarE dv)
          []
        , TH.Clause [TH.VarP ev, TH.VarP tv, conP 'PGBinaryValue [TH.VarP dv]]
          (TH.NormalB $ TH.VarE 'pgDecodeBinary `TH.AppE` TH.VarE ev `TH.AppE` TH.VarE tv `TH.AppE` TH.VarE dv)
          []
        ]
      ]
#endif
    , instanceD [] (TH.ConT ''PGRep `TH.AppT` typt)
      [ tySynInstD ''PGRepType typt typl
      ]
    , instanceD [] (TH.ConT ''PGRecordType `TH.AppT` typl) []
    , instanceD [] (TH.ConT ''PGRelation `TH.AppT` typt)
      [ TH.FunD 'pgRelationName [TH.Clause [TH.WildP]
        (TH.NormalB $ namelit pgtab)
        [] ]
      , TH.FunD 'pgColumnNames [TH.Clause [TH.WildP]
        (TH.NormalB $ TH.ListE $ map (\(c, _, _, _) -> namelit c) cold)
        [] ]
      ]
    , TH.SigD (TH.mkName ("uncurry" ++ typs)) $ TH.ArrowT `TH.AppT`
      foldl (\f (_, t, n) -> f `TH.AppT`
          (if n then (TH.ConT ''Maybe `TH.AppT`) else id)
          (TH.ConT ''PGVal `TH.AppT` t))
        (TH.ConT (TH.tupleTypeName (length cols)))
        cols `TH.AppT` typt
    , TH.FunD (TH.mkName ("uncurry" ++ typs))
      [ TH.Clause [conP (TH.tupleDataName (length cols)) (map (\(v, _, _) -> TH.VarP v) cols)]
        (TH.NormalB $ foldl (\f (v, _, _) -> f `TH.AppE` TH.VarE v) (TH.ConE typn) cols)
        []
      ]
    , TH.PragmaD $ TH.AnnP (TH.TypeAnnotation typn) $ namelit pgid
    , TH.PragmaD $ TH.AnnP (TH.ValueAnnotation typn) $ namelit pgid
    ] ++ map (\(c, n, _, _) ->
      TH.PragmaD $ TH.AnnP (TH.ValueAnnotation n) $ namelit c) cold
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
  pgcall f t = TH.VarE f `TH.AppE`
    (TH.ConE 'PGTypeProxy `TH.SigE`
      (TH.ConT ''PGTypeID `TH.AppT` t))
  colenc f (v, t, False) = TH.ConE 'Just `TH.AppE` (pgcall f t `TH.AppE` TH.VarE v)
  colenc f (v, t, True) = TH.VarE 'fmap `TH.AppE` pgcall f t `TH.AppE` TH.VarE v
  colpat (v, _, False) = conP 'Just [TH.VarP v]
  colpat (v, _, True) = TH.VarP v
  coldec (v, t, False) = pgcall 'pgDecode t `TH.AppE` TH.VarE v
  coldec (v, t, True) = TH.VarE 'fmap `TH.AppE` pgcall 'pgDecode t `TH.AppE` TH.VarE v
  rect = TH.LitT $ TH.StrTyLit "record"
  namelit n = TH.ConE 'PGName `TH.AppE`
    TH.ListE (map (TH.LitE . TH.IntegerL . fromIntegral) $ pgNameBytes n)
  conP n p = TH.ConP n
#if MIN_VERSION_template_haskell(2,18,0)
    []
#endif
    p
