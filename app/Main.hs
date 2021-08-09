module Main where

import qualified Data.ByteString  as ByteString ( getLine, getContents, length )
import qualified Numeric
import qualified System.Environment as Environment
import Crc
import Crc.Register32
import qualified Crc.BitsExtenshions as BitsExt
import Control.Monad

main :: IO ()
main = do
    args <- Environment.getArgs
    let stdinArg = "--stdin"
    if stdinArg `elem` args 
        then fromStdIn 
        else fromCLI
    
fromStdIn = ByteString.getContents >>= print . BitsExt.d2h . crc32

fromCLI = do
    print "Enter data: "
    line <- ByteString.getLine 
    putStr "CRC: "
    print (BitsExt.d2h $ crc32 line)
    