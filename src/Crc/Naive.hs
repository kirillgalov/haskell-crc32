{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Crc.Naive where

import Crc
import Data.Bits
import qualified Crc.BitsExtenshions as BitsExt
import Control.Monad.State.Lazy
import Data.Word


newtype CRC32_Naive = CRC32_Naive {bytes :: [Word8]}

instance Crc32 CRC32_Naive where
    crc32 CRC32_Naive {..} = xor 0xFFFFFFFF $ bitReverse32 $ rbits $ execState (pushWords polinom bytes) initRegister where
        polinom = Polinom (Register32 0x4C11DB7)
        initRegister = Register32 0xFFFFFFFF



type Bit = Bool
newtype Register32 = Register32 {rbits :: Word32} deriving (Show, Eq, Bits, FiniteBits)
newtype Polinom = Polinom {preg :: Register32} deriving (Show, Eq, Bits, FiniteBits)



pushWords :: (Foldable t, FiniteBits b) => Polinom -> t b -> State Register32 ()
pushWords p = mapM_ (pushBits p)

pushBits :: (FiniteBits b) => Polinom -> b -> State Register32 ()
pushBits p bits = pushBits' (finiteBitSize bits) p bits  where
    pushBits' 0 _ _ = return ()
    pushBits' n p bits  = do
        exitBit <- pushBit (BitsExt.testLeftBit bits)
        reg <- get
        when exitBit (put $ xor (preg p) reg)
        pushBits' (n - 1) p (BitsExt.shiftl1 bits)


pushBit :: Bit -> State Register32 Bit
pushBit bit = do
    register <- get
    put $ BitsExt.apply0Bit bit $ BitsExt.shiftl1 register
    return $ BitsExt.testLeftBit register