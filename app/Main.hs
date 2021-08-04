module Main where

import qualified Data.ByteString  as ByteString ( getLine, getContents )
import qualified Numeric
import CRC32 ( crc32 )
import qualified System.Environment as Environment
--import Data.List

main :: IO ()
main = do
    args <- Environment.getArgs
    datas <- if "--stdin" `elem` args then ByteString.getContents else ByteString.getLine
    print $ toHex $ crc32 datas


toHex :: (Integral a, Show a) => a -> String
toHex value = Numeric.showHex value ""