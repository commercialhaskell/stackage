{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, DataKinds, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module: Database.PostgreSQL.Typed.Inet
-- Copyright: 2015 Dylan Simon
-- 
-- Representaion of PostgreSQL's inet/cidr types using "Network.Socket".
-- We don't (yet) supply PGColumn (parsing) instances.

module Database.PostgreSQL.Typed.Inet where

import           Control.Monad (void, guard, liftM2)
import qualified Data.ByteString.Char8 as BSC
import           Data.Bits (shiftL, (.|.))
import           Data.Maybe (fromJust)
import           Data.Word (Word8, Word16, Word32)
import           Foreign.Marshal.Array (withArray)
import           Foreign.Ptr (castPtr)
import           Foreign.Storable (peek)
import qualified Network.Socket as Net
import           Numeric (readDec, readHex)
import           System.IO.Unsafe (unsafeDupablePerformIO)
import qualified Text.ParserCombinators.ReadP as RP
import qualified Text.ParserCombinators.ReadPrec as RP (lift)
import           Text.Read (Read(readPrec))

import Database.PostgreSQL.Typed.Types

data PGInet 
  = PGInet
    { pgInetAddr :: !Net.HostAddress
    , pgInetMask :: !Word8
    }
  | PGInet6
    { pgInetAddr6 :: !Net.HostAddress6
    , pgInetMask :: !Word8
    }
  deriving (Eq)

sockAddrPGInet :: Net.SockAddr -> Maybe PGInet
sockAddrPGInet (Net.SockAddrInet _ a) = Just $ PGInet a 32
sockAddrPGInet (Net.SockAddrInet6 _ _ a _) = Just $ PGInet6 a 128
sockAddrPGInet _ = Nothing

-- |Convert four bytes to network byte order, using unsafe casting.
-- 'Data.Word.byteSwap32' would be better, but I couldn't find a good way to determine host byte order.
bton32 :: (Word8, Word8, Word8, Word8) -> Word32
bton32 (b1, b2, b3, b4) = unsafeDupablePerformIO $
  withArray [b1, b2, b3, b4] (peek . castPtr)

instance Show PGInet where
  -- This is how Network.Socket's Show SockAddr does it:
  show (PGInet a 32) = fromJust $ fst $ unsafeDupablePerformIO $
    Net.getNameInfo [Net.NI_NUMERICHOST] True False (Net.SockAddrInet 0 a)
  show (PGInet a m) = show (PGInet a 32) ++ '/' : show m
  show (PGInet6 a 128) = fromJust $ fst $ unsafeDupablePerformIO $
    Net.getNameInfo [Net.NI_NUMERICHOST] True False (Net.SockAddrInet6 0 0 a 0)
  show (PGInet6 a m) = show (PGInet6 a 128) ++ '/' : show m

instance Read PGInet where
  -- This is even less pleasant, but we only have to deal with representations pg generates
  -- Not at all efficient, since in ReadP, but should get us by
  readPrec = RP.lift $ r4 RP.+++ r6 where
    r4i = do
      o1 <- rdec
      _ <- RP.char '.'
      o2 <- rdec
      _ <- RP.char '.'
      o3 <- rdec
      _ <- RP.char '.'
      o4 <- rdec
      return (o1, o2, o3, o4)
    -- ipv4
    r4 = do
      q <- r4i
      m <- mask 32
      return $ PGInet (bton32 q) m

    -- trailing ipv4 in ipv6
    r64 = do
      (b1, b2, b3, b4) <- r4i
      return [jb b1 b2, jb b3 b4]
    -- ipv6 pre-double-colon
    r6l 0 = return []
    r6l 2 = colon >> r6lc 2 RP.+++ r64
    r6l n = colon >> r6lc n
    r6lc n = r6lp n RP.+++ r6b n
    r6lp n = r6w (r6l (pred n))
    -- ipv6 double-colon
    r6b n = do
      colon
      r <- r6rp (pred n) RP.<++ return []
      let l = length r
      return $ replicate (n - l) 0 ++ r
    -- ipv6 post-double-colon
    r6r 0 = return []
    r6r n = (colon >> r6rp n) RP.<++ return []
    r6rp n
      | n >= 2 = r6rc n RP.+++ r64
      | otherwise = r6rc n
    r6rc n = r6w (r6r (pred n))
    r6w = liftM2 (:) rhex
    -- ipv6
    r6 = do
      [w1, w2, w3, w4, w5, w6, w7, w8] <- r6lp 8 RP.<++ (colon >> r6b 8)
      m <- mask 128
      return $ PGInet6 (jw w1 w2, jw w3 w4, jw w5 w6, jw w7 w8) m

    colon = void $ RP.char ':'
    mask m = RP.option m $ do
      _ <- RP.char '/'
      n <- rdec
      guard (n <= m)
      return n
    rdec :: RP.ReadP Word8
    rdec = RP.readS_to_P readDec
    rhex :: RP.ReadP Word16
    rhex = RP.readS_to_P readHex
    jw :: Word16 -> Word16 -> Word32
    jw x y = fromIntegral x `shiftL` 16 .|. fromIntegral y
    jb :: Word8 -> Word8 -> Word16
    jb x y = fromIntegral x `shiftL` 8 .|. fromIntegral y

instance PGType "inet" where
  type PGVal "inet" = PGInet
instance PGType "cidr" where
  type PGVal "cidr" = PGInet
instance PGParameter "inet" PGInet where
  pgEncode _ = BSC.pack . show
instance PGParameter "cidr" PGInet where
  pgEncode _ = BSC.pack . show
instance PGColumn "inet" PGInet where
  pgDecode _ = read . BSC.unpack
instance PGColumn "cidr" PGInet where
  pgDecode _ = read . BSC.unpack
