module Crc where

import Control.Monad.State.Lazy
import Data.Bits
import Data.ByteString (ByteString, unpack)
import qualified Crc.BitsExtenshions as BitsExt




type Bit = Bool
newtype Polinom r = Polinom r



class (FiniteBits r) => Register r where

    initRegister :: r

    merge :: Polinom r -> r -> r
    merge (Polinom r) reg = xor r reg

    pushBits :: (FiniteBits b) => Polinom r -> b -> State r ()
    pushBits p bits = pushBits' (finiteBitSize bits) p bits  where
        pushBits' 0 _ _ = return ()
        pushBits' n p bits  = do
            exitBit <- pushBit (BitsExt.testLeftBit bits)
            reg <- get
            when exitBit (put $ merge p reg)
            pushBits' (n - 1) p (BitsExt.shiftl1 bits)

    pushBit :: Bit -> State r Bit
    pushBit bit = do
        register <- get
        put $ BitsExt.apply0Bit bit $ BitsExt.shiftl1 register
        return $ BitsExt.testLeftBit register

    pushWords :: (Foldable t, FiniteBits b) => Polinom r -> t b -> State r ()
    pushWords p = mapM_ (pushBits p)

crc :: (Register r) => Polinom r -> ByteString -> r
crc polinom input = execState (pushWords polinom (unpack input)) initRegister