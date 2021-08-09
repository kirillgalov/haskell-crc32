module Main where

import System.IO
import qualified System.Environment as Environment
import qualified Data.ByteString  as BS

import Crc
import qualified Crc.Naive as Naive
import qualified Crc.BitsExtenshions as BitsExt
import qualified Crc.CRC_32_IEEE_802_3 as IEEE

main :: IO ()
main = do
    args <- Environment.getArgs
    let stdinArg = "--stdin"
    let naiveArg = "--naive"

    let computeCrc32 bytes = BitsExt.dec2hex $ if naiveArg `elem` args 
        then crc32 $ Naive.CRC32_Naive bytes
        else crc32 $ IEEE.CRC_32_IEEE_802_3 bytes

    let inputSource = if stdinArg `elem` args 
        then fromStdIn
        else fromCLI

    inputSource computeCrc32
    


fromStdIn compute = BS.getContents >>= print . compute . BS.unpack

fromCLI compute = do
    line <- promt "Enter data: " 
    putStr "CRC: "
    putStrLn $ compute $ BS.unpack line

promt str = putStr str >> hFlush stdout >> BS.getLine 
    