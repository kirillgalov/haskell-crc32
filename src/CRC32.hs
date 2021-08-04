{-# LANGUAGE OverloadedStrings, GeneralisedNewtypeDeriving #-}

module CRC32 where


import Control.Monad.State.Lazy
import Data.Bits
import Data.ByteString (ByteString, unpack, getLine)
import Data.Word ( Word8 )
import qualified Numeric


newtype Polinom = Polinom {pbits :: Int} deriving (Eq, Show, Bits, FiniteBits)
newtype Register = Register {rbits :: Int} deriving (Eq, Bits, FiniteBits)
type Bit = Bool



emptyRegister :: Register
emptyRegister = Register zeroBits
sPolinom :: Polinom
sPolinom = pow2Polinom [1,3,6,7,9,10,15,20,29,30]

pow2Polinom :: [Int] -> Polinom
pow2Polinom = Polinom . foldl setBit zeroBits

pushBit :: Bit -> State Register Bit
pushBit bit = do
    register <- get
    put $ apply0Bit bit $ shiftl1 register
    return $ testLeftBit register

pushWord8 :: FiniteBits b => b -> Polinom -> State Register ()
pushWord8 word p = pushWord8' word p (finiteBitSize word) where
    pushWord8' _ _ 0 = return ()
    pushWord8' word p n = do
        exitBit <- pushBit (testLeftBit word)
        reg <- get
        when exitBit (put $ merge p reg)
        pushWord8' (shiftl1 word) p (n - 1)


testLeftBit :: FiniteBits a => a -> Bool
testLeftBit a = testBit a (finiteBitSize a - 1)

shiftl1 :: Bits a => a -> a
shiftl1 = flip shift 1

apply0Bit :: Bits b => Bool -> b -> b
apply0Bit b = if b then flip setBit 0 else flip clearBit 0

merge :: Polinom -> Register -> Register
merge p r = Register $ xor (rbits r) (pbits p)


crc :: Polinom -> ByteString -> Int
crc polinom input = rbits $ execState (pushWords (unpack input) polinom) emptyRegister

crc32 :: ByteString -> Int
crc32 = crc sPolinom

pushWords :: [Word8] -> Polinom -> State Register ()
pushWords [] _ = return ()
pushWords (w:ws) p = pushWord8 w p >> pushWords ws p

-- pushWords2 :: [Word8] -> Polinom -> State Register Bit
-- pushWords2 ws p = foldM push False ws where
--     push _ word = pushWord8 word p

