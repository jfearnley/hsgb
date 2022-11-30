module IO where

import Data.Word
import Data.Char
import Data.Bits
import qualified Data.Set as Set

import Types
import CpuBasic

---- Joypad Keys

-- Converts a list of bits, given from LSB to MSB, to a Word8
boolsToWord8 :: [Bool] -> Word8
boolsToWord8 bools = sum $ zipWith bitf bools [0..]
    where bitf b i = if b then bit i else 0

-- Reading from keys
readP1 :: GBState s Word8
readP1 = do
    p1   <- askRef regP1
    keys <- askRef keys
    let p14 = testBit p1 4
        p15 = testBit p1 5
        keyf x p = not p && Set.member x keys
        linef x y = not $ keyf x p14 || keyf y p15
        p10 = linef KeyRight KeyA
        p11 = linef KeyLeft  KeyB
        p12 = linef KeyUp    KeySelect
        p13 = linef KeyDown  KeyStart
    return $ boolsToWord8 [p10, p11, p12, p13, p14, p15, True, True]


---- Serial Port

writeSerial :: Word8 -> GBState s ()
writeSerial = putRef serialChar 

writeSerialC :: Word8 -> GBState s ()
writeSerialC _ = do
    c <- askRef serialChar
    buff <- askRef serialBuffer
    let new_char = chr . fromEnum $ c
        new_buff = buff ++ [new_char]
    putRef serialBuffer new_buff
