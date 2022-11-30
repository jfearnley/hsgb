module Timer where

import Control.Monad.State.Strict
import Data.Bits
import Data.Word

import Types
import CpuBasic
import Util



---- DIV register

writeDIV :: Word8 -> GBState s ()
writeDIV _ = readCycles >>= (putRef lastDIVWrite)


readDIV :: GBState s Word8
readDIV = do
    cycles <- readCycles
    last   <- askRef lastDIVWrite
    let w16    = fromIntegral $ (cycles - last) `mod` 0xFFFF 
        (_, h) = from16 w16    
    return h


---- TAC register

timerEnabled :: GBState s Bool
timerEnabled = do
    tac <- readTAC
    return $ testBit tac 2

timerClock :: GBState s Integer
timerClock = do
    tac <- readTAC
    return $ case (testBit tac 1, testBit tac 0) of
                (False, False) -> 1024
                (False, True)  -> 16
                (True,  False) -> 64
                (True,  True)  -> 256

writeTAC :: Word8 -> GBState s ()
writeTAC val = do
    old <- readTAC
    putRef regTAC (val .|. 0xF8) 

    -- Has timer just been started? 
    when (testBit val 2 && not (testBit old 2)) 
        $ readCycles >>= (putRef timerStarted)

    -- Has timer just been stopped?
    when (not (testBit val 2) && testBit old 2) $ readTIMA >>= (putRef regTIMA)


---- TIMA register

writeTIMA :: Word8 -> GBState s ()
writeTIMA val = do
    putRef regTIMA val
    readCycles >>= putRef timerStarted

readTIMA :: GBState s Word8
readTIMA = do
    enabled <- timerEnabled
    if not enabled then fromIntegral <$> askRef regTIMA
    else do
        init    <- askRef regTIMA
        started <- askRef timerStarted
        current <- readCycles
        rate    <- timerClock
        return $ init + fromIntegral ((current - started) `div` rate)


