module Crc.BitsExtenshions where

import Data.Bits


apply0Bit :: (Bits b) => Bool -> b -> b
apply0Bit bit bits = if bit then setBit bits 0 else clearBit bits 0


testLeftBit :: FiniteBits b => b -> Bool
testLeftBit bits = testBit bits (finiteBitSize bits - 1)


shiftl1 ::FiniteBits b => b -> b
shiftl1 = flip shift 1

dec2char :: Integral a => a -> Char
dec2char n = "0123456789ABCDEF" !! fromIntegral n

dec2hex' :: (Integral t, Bits t) => t -> [Char]
dec2hex' dec = dec2char (15 .&. dec) : if dec == 0 then "" else dec2hex' (ss dec) where 
   ss b = complement (shiftL 15 28) .&. shiftR b 4

to2 :: Bits a => a -> [Char]
to2 b = reverse $ map (\ n -> if testBit b n then '1' else '0') [0..31]

dec2hex :: (Integral t, Bits t) => t -> [Char]
dec2hex dec = reverse res where
    unp = dec2hex' dec
    lenunp = length unp
    res = unp ++ replicate (8 - lenunp) '0' ++ "x0"
