-- |
-- Module: Database.PostgreSQL.Typed.SQLToken
-- Copyright: 2016 Dylan Simon
-- 
-- Parsing of SQL statements to safely identify placeholders.
-- Supports both dollar-placeholders and question marks for HDBC.
{-# LANGUAGE PatternGuards #-}
module Database.PostgreSQL.Typed.SQLToken
  ( SQLToken(..)
  , sqlTokens
  ) where

import Control.Arrow (first)
import Data.Char (isDigit, isAsciiUpper, isAsciiLower)
import Data.List (stripPrefix)
import Data.String (IsString(..))

-- |A parsed SQL token.
data SQLToken
  = SQLToken String -- ^Raw (non-markup) SQL string
  | SQLParam Int -- ^A \"$N\" parameter placeholder (this is the only non-string-preserving token: \"$012\" becomes \"$12\")
  | SQLExpr String -- ^A \"${expr}\" expression placeholder
  | SQLQMark Bool -- ^A possibly-escaped question-mark: False for \"?\" or True for \"\\?\"
  deriving (Eq)

-- |Produces the original SQL string
instance Show SQLToken where
  showsPrec _ (SQLToken s) = showString s
  showsPrec _ (SQLParam p) = showChar '$' . shows p
  showsPrec _ (SQLExpr e) = showString "${" . showString e . showChar '}'
  showsPrec _ (SQLQMark False) = showChar '?'
  showsPrec _ (SQLQMark True) = showString "\\?"
  showList = flip $ foldr shows

instance IsString SQLToken where
  fromString = SQLToken

type PH = String -> [SQLToken]

infixr 4 ++:, +:

(++:) :: String -> [SQLToken] -> [SQLToken]
p ++: (SQLToken q : l) = SQLToken (p ++ q) : l
p ++: l = SQLToken p : l

(+:) :: Char -> [SQLToken] -> [SQLToken]
p +: (SQLToken q : l) = SQLToken (p : q) : l
p +: l = SQLToken [p] : l

x :: PH
x ('-':'-':s) = "--" ++: comment s
x ('e':'\'':s) = "e'" ++: xe s
x ('E':'\'':s) = "E'" ++: xe s
x ('\'':s) = '\'' +: xq s
x ('$':'{':s) = expr s
x ('$':'$':s) = "$$" ++: xdolq "" s
x ('$':c:s)
  | dolqStart c
  , (t,'$':r) <- span dolqCont s
  = '$' : c : t ++: '$' +: xdolq (c:t) r
  | isDigit c
  , (i,r) <- span isDigit s
  = SQLParam (read $ c:i) : x r
x ('"':s) = '"' +: xd s
x ('/':'*':s) = "/*" ++: xc 1 s
x (c:s)
  | identStart c
  , (i,r) <- span identCont s
  = c : i ++: x r
x ('\\':'?':s) = SQLQMark True : x s
x ('?':s) = SQLQMark False : x s
x (c:s) = c +: x s
x [] = []

xthru :: (Char -> Bool) -> PH
xthru f s = case break f s of
  (p, c:r) -> p ++ [c] ++: x r
  (p, []) -> [SQLToken p]

comment :: PH
comment = xthru (\n -> '\n' == n || '\r' == n)

xe :: PH
xe ('\\':c:s) = '\\' +: c +: xe s
xe ('\'':s) = '\'' +: x s
xe (c:s) = c +: xe s
xe [] = []

xq :: PH
xq = xthru ('\'' ==)
-- no need to handle xqdouble

xd :: PH
xd = xthru ('\"' ==)
-- no need to handle xddouble

identStart, identCont, dolqStart, dolqCont :: Char -> Bool
identStart c = isAsciiUpper c || isAsciiLower c || c >= '\128' && c <= '\255' || c == '_'
dolqStart = identStart
dolqCont c = dolqStart c || isDigit c
identCont c = dolqCont c || c == '$'

xdolq :: String -> PH
xdolq t = dolq where
  dolq ('$':s)
    | Just r <- stripPrefix t' s = '$':t' ++: x r
  dolq (c:s) = c +: dolq s
  dolq [] = []
  t' = t ++ "$"

xc :: Int -> PH
xc 0 s = x s
xc n ('/':'*':s) = "/*" ++: xc (succ n) s
xc n ('*':'/':s) = "*/" ++: xc (pred n) s
xc n (c:s) = c +: xc n s
xc _ [] = []

expr :: PH
expr = pr . ex (0 :: Int) where
  pr (e, Nothing) = [SQLToken ("${" ++ e)]
  pr (e, Just r) = SQLExpr e : r
  ex 0 ('}':s) = ("", Just $ x s)
  ex n ('}':s) = first ('}':) $ ex (pred n) s
  ex n ('{':s) = first ('{':) $ ex (succ n) s
  ex n (c:s) = first (c:) $ ex n s
  ex _ [] = ("", Nothing)

-- |Parse a SQL string into a series of tokens.
-- The 'showList' implementation for 'SQLToken' inverts this sequence back to a SQL string.
sqlTokens :: String -> [SQLToken]
sqlTokens = x
