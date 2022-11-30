module Util where

import Data.Word
import Data.Int
import Data.Char
import Numeric

to16 :: Word8 -> Word8 -> Word16
to16 low high = fromIntegral high * 256 + fromIntegral low

from16 :: Word16 -> (Word8, Word8)
from16 val = (fromIntegral $ val `mod` 256, fromIntegral $ val `div` 256)


toHex :: Word8 -> String
toHex n = case h of [x] -> ['0', x]
                    _   -> h
    where h = map toUpper $ showHex n ""

toHex16 :: Word16 -> String
toHex16 n = replicate (4 - length h) '0' ++ h
    where h = map toUpper $ showHex n ""

fromHex :: (Num a, Eq a) => String -> a
fromHex = fst . head . readHex 

signed :: Word8 -> Int8
signed = fromIntegral

toScreen :: Int -> Int -> Int
toScreen x y = y * 160 + x

fromScreen :: Int -> (Int, Int)
fromScreen idx = (x, y)
    where x = idx `mod` 160
          y = idx `div` 160
