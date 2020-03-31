{- | crc16 ccitt

   1) MSB first 1021 x^16 + x^12 + x^5 + 1 

   2) LSB first 8048

-}
module Data.Digest.CRC16 (
     -- * CRC16 method
     crc16_update
    ) where

import Data.Word(Word8,Word16)
import Data.Bits


-- | crc16 calculation
-- This uses the simple method based on /bit shifting/.
-- See the unittests for an example.
--
crc16_update :: Word16      -- ^ polynomial
             -> Bool        -- ^ inverse bits 
             -> Word16      -- ^ initial crc
             -> Word8       -- ^ data byte
             -> Word16      -- ^ new crc
crc16_update poly rev crc b =
    foldl (crc16_update_bit poly) new_crc [1..(bitSize b)]
    where
        new_crc = crc `xor` (shiftL (fromIntegral b' :: Word16) 8 )
        b' = if rev
                then reverse_bits b
                else b




crc16_update_bit :: Word16 -> Word16 -> Int -> Word16
crc16_update_bit poly crc _ =
    if (crc .&. 0x8000) /= 0x0000
        then (shiftL crc 1) `xor` poly 
        else shiftL crc 1



-- | Reverse the bits in a byte
--
-- 7..0 becomes 0..7
--
reverse_bits :: Word8 -> Word8
reverse_bits b =
    (shiftL (b .&. 0x01) 7) .|. (shiftL (b .&. 0x02) 5) .|. (shiftL (b .&. 0x04) 3) .|. (shiftL (b .&. 0x08) 1) .|.
    (shiftR (b .&. 0x10) 1) .|. (shiftR (b .&. 0x20) 3) .|. (shiftR (b .&. 0x40) 5) .|. (shiftR (b .&. 0x80) 7)

