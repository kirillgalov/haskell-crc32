{-# LANGUAGE BangPatterns #-}
module Crc.BitsExtenshions where

import Data.Bits
import Data.Int


apply0Bit :: (Bits b) => Bool -> b -> b
apply0Bit bit bits = if bit then setBit bits 0 else clearBit bits 0


testLeftBit :: FiniteBits b => b -> Bool
testLeftBit bits = testBit bits (finiteBitSize bits - 1)


shiftl1 ::FiniteBits b => b -> b
shiftl1 = flip shift 1


dec2char :: Int32 -> Char
dec2char n = "0123456789ABCDEF" !! fromIntegral n

--dec2hex :: Int -> Int -> String
dec2hex :: Int32 -> [Char]
dec2hex dec = dec2char (15 .&. dec) : if dec == 0 then "" else dec2hex (ss dec)

to2 b = reverse $ map (\ n -> if testBit b n then '1' else '0') [0..31]

d2h dec = reverse res where
    unp = dec2hex dec
    lenunp = length unp
    res = unp ++ replicate (8 - lenunp) '0' ++ "x0"
low2hex b = "0123456789ABCDEF" !! (16 .&. b)

ss :: Int32 -> Int32
ss b = complement (shiftL 15 28) .&. shiftR b 4

inverseOrder ::(FiniteBits b) => b -> b
inverseOrder bits = foldl (\acc nBit -> if testBit bits nBit then setBit acc (size - nBit) else clearBit acc (size - nBit)) zeroBits  [0..finiteBitSize bits - 1] where
    size = finiteBitSize bits - 1


-- https://hackage.haskell.org/package/haskus-binary-1.0/docs/src/Haskus.Format.Binary.Bits.Reverse.html#reverseBits5LgN
reverseBits :: FiniteBits a => a -> a
reverseBits x = rec (finiteBitSize x `shiftR` 1) (complement zeroBits) x
   where
      rec :: FiniteBits a => Int -> a -> a -> a
      rec !s !mask !v
         | s <= 0        = v
         | otherwise     = rec (s `shiftR` 1) mask' v'
            where
               mask' = mask `xor` (mask `shiftL` s)
               v'    =      ((v `shiftR` s) .&. mask')
                        .|. ((v `shiftL` s) .&. complement mask')

