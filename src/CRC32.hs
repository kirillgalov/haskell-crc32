{-# LANGUAGE OverloadedStrings, StandaloneDeriving, GeneralisedNewtypeDeriving #-}

module CRC32 (crc32, emptyRegister, sPolinom, rbits) where



import Control.Monad.State.Lazy
import Data.Bits
import Data.ByteString (ByteString, unpack, getLine)
import Data.Word ( Word8 )
import qualified Numeric


newtype Polinom = Polinom {pbits :: Int} deriving (Eq, Show)

newtype Register = Register {rbits :: Int} deriving (Eq)
type Bit = Bool

instance Show Register where
    show = flip Numeric.showHex "" . rbits

deriving instance Bits Polinom
deriving instance Bits Register
deriving instance FiniteBits Polinom
deriving instance FiniteBits Register


emptyRegister :: Register
emptyRegister = Register zeroBits
sPolinom :: Polinom
sPolinom = pow2Polinom [1,3,6,7,9,10,15,20,29,30]

pow2Polinom :: [Int] -> Polinom
pow2Polinom = Polinom . foldl setBit zeroBits

pushBit :: Bit -> State Register Bit
pushBit bit = do
    register <- get
    let (shifted, exitBit) = pushBit2Register register bit
    put shifted
    return exitBit

pushWord8 :: Word8 -> Polinom -> State Register Bit
pushWord8 0 _ = return False
pushWord8 word p = do
    let (fbit, tbits) = split word
    exitBit <- pushBit fbit
    reg <- get
    when exitBit (put $ merge p reg)
    pushWord8 tbits p

pushBit2Register :: Register -> Bit -> (Register, Bit)
pushBit2Register reg nb = (shiftedReg, exitedBit) where
    shiftedReg = apply0Bit nb $ shiftl1 reg
    exitedBit = testLeftBit reg

testLeftBit :: FiniteBits a => a -> Bool
testLeftBit a = testBit a (finiteBitSize a - 1)

shiftl1 :: Bits a => a -> a
shiftl1 = flip shift 1

apply0Bit :: Bits b => Bool -> b -> b
apply0Bit b = if b then flip setBit 0 else flip clearBit 0

split :: Word8 -> (Bool, Word8)
split w = (testLeftBit w, shiftl1 w)

merge :: Polinom -> Register -> Register
merge p r = Register $ xor (rbits r) (pbits p)

crc32 :: Polinom -> ByteString -> Int
crc32 polinom input = rbits $ execState (pushWords (unpack input) polinom) emptyRegister


pushWords :: [Word8] -> Polinom -> State Register Bit
pushWords [] _ = return False
pushWords (w:ws) p = pushWord8 w p >> pushWords ws p

pushWords2 :: [Word8] -> Polinom -> State Register Bit
pushWords2 ws p = foldM push False ws where
    push _ word = pushWord8 word p

