{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Crc.Register32  where

import Crc
import Data.ByteString
import Data.Bits
import qualified Crc.BitsExtenshions as BitsExt
import Control.Monad.State.Lazy
import Data.Foldable
import Data.Int

newtype Register32 = Register32 {rbits :: Int32} deriving (Show, Eq, Bits, FiniteBits)



instance Register Register32 where
    initRegister = Register32 $ complement zeroBits


crc32 :: ByteString -> Int32
crc32 = complement . BitsExt.reverseBits . rbits . crc crc32cPolinom
--crc32 = rbits . crc crc32cPolinom

crc32cPolinom = Polinom (Register32 0x4C11DB7)

