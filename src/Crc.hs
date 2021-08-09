module Crc where

import Data.Word

class Crc32 c where
    crc32 :: c -> Word32