module Main where

import Test.HUnit.Base
import Test.HUnit.Text
import Data.Digest.CRC16
import qualified Data.ByteString as B
import Data.Word


crc16 :: Word16 -> Bool -> Word16 -> [Word8] -> Word16
crc16 poly inverse initial data_string =
    B.foldl (crc16_update poly inverse) initial (B.pack data_string)

--
-- Checkout http://zorc.breitbandkatze.de/crc.html
tests :: Test
tests = "Basic" ~: [
             0x29B1 ~=? crc16 0x1021 False 0xffff simple_string
            ,0x89f6 ~=? crc16 0x1021 True 0xffff simple_string
            ,0x31C3 ~=? crc16 0x1021 False 0x0000 simple_string
        ]
        where 
            simple_string = [0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39] --string "123456789"
main = runTestTT tests
