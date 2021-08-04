module Main where

import Data.ByteString ( getLine, ByteString )
import CRC32 ( crc32, sPolinom )


main :: IO ()
main = Data.ByteString.getLine >>= print . programm

programm :: ByteString -> Int
programm = crc32 sPolinom