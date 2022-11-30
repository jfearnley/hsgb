module CpuBasic where

import Control.Monad
import Control.Monad.Reader
import Data.Word
import Data.Bits
import Data.STRef
import qualified Data.Set as Set

import Types
import Util

---- Reading registers

askRef :: (STGB s -> STRef s a) -> GBState s a
askRef reg = asks reg >>= lift . readSTRef 
{-# INLINE askRef #-}

[readA, readB, readC, readD, readE, readH, readL] = 
    map askRef [regA, regB, regC, regD, regE, regH, regL]

readF :: Read8 s
readF = do
    z <- getZero
    n <- getNegative
    h <- getHCarry
    c <- getCarry
    return $ ind z * 128 + ind n * 64 + ind h * 32 + ind c * 16
    where ind x = if x then 1 else 0

regBit :: GBState s Word8 -> Int -> GBState s Bool
regBit reg i = fmap (`testBit` i) reg

[readSP, readPC] = map askRef [regSP, regPC]


readCompound :: Read8 s -> Read8 s -> Read16 s
readCompound = liftM2 to16 

[readBC, readDE, readHL, readAF] = map (uncurry readCompound) 
    [(readC, readB), (readE, readD), (readL, readH), (readF, readA)]



---- Writing registers

putRef :: (STGB s -> STRef s a) -> a -> GBState s ()
putRef reg val = do
    ref <- asks reg
    lift $ writeSTRef ref val
{-# INLINE putRef #-}

modifyRef :: (STGB s -> STRef s a) -> (a -> a) -> GBState s ()
modifyRef reg f = putRef reg =<< fmap f (askRef reg)

[writeA, writeB, writeC, writeD, writeE, writeH, writeL] = 
    map putRef [regA, regB, regC, regD, regE, regH, regL]

[writeSP, writePC] = map putRef [regSP, regPC]

writeF :: Write8 s
writeF value = do
    setZero $ t 7
    setNegative $ t 6
    setHCarry $ t 5
    setCarry $ t 4
    where t = testBit value 


writeCompound :: Write8 s -> Write8 s -> Write16 s
writeCompound low high val = do {low l; high h}
    where (l, h) = from16 val

[writeBC, writeDE, writeHL, writeAF] = map (uncurry writeCompound) 
    [(writeC, writeB), (writeE, writeD), (writeL, writeH), (writeF, writeA)]


---- Flags


[getCarry, getHCarry, getZero, getNegative] = 
    map askRef [carry, hcarry, zero, negative]

[setCarry, setHCarry, setZero, setNegative] = 
    map putRef [carry, hcarry, zero, negative]


setFlags :: Bool -> Bool -> Bool -> Bool -> GBState s ()
setFlags negative zero carry hcarry = 
    setNegative negative >> setZero zero >> setCarry carry >> setHCarry hcarry

---- Conditions for jumps etc.


invert :: GBState s Bool -> GBState s Bool
invert = fmap not 

always :: GBState s Bool
always = return True

nzCond = invert getZero
zCond  = getZero
ncCond = invert getCarry
cCond  = getCarry

---- Cycle count functions

readCycles = askRef cycles

writeCycles = putRef cycles


incCyc :: Integer -> GBState s ()
incCyc n = readCycles >>= \c -> writeCycles (c + n) 
{-# INLINE incCyc #-}

decCyc :: Integer -> GBState s ()
decCyc n = readCycles >>= \c -> writeCycles (c - n) 
{-# INLINE decCyc #-}

incPC :: Word16 -> GBState s ()
incPC n = readPC >>= \pc -> writePC (pc + n)
{-# INLINE incPC #-}

decPC :: Word16 -> GBState s ()
decPC n = readPC >>= \pc -> writePC (pc - n)
{-# INLINE decPC #-}

---- Interrupts

[readIE, readIF] = map askRef [regIE, regIF]

readIME = askRef ime


[writeIE, writeIF] = map putRef [regIE, regIF]

writeIME = putRef ime

markEventsStale = putRef eventsStale True

---- GPU

[readLCDC, readSCY, readSCX, readBGP, readWY, readWX, readOBP0, readOBP1, readLYC] = 
    map askRef [regLCDC, regSCY, regSCX, regBGP, regWY, regWX, regOBP0, regOBP1, regLYC]

[writeLCDC, writeSCY, writeSCX, writeBGP, writeWY, writeWX, writeOBP0, writeOBP1, writeLYC] = 
    map putRef [regLCDC, regSCY, regSCX, regBGP, regWY, regWX, regOBP0, regOBP1, regLYC]



setIFBit :: Int -> GBState s ()
setIFBit i = readIF >>= (\x -> return $ setBit x i) >>= writeIF


readLY :: GBState s Word8
readLY = do
    fs  <- askRef frameStart
    cyc <- askRef cycles
    let answer = (cyc - fs) `div` 456
    return $ fromIntegral answer


readSTATBit :: Int -> GBState s Bool
readSTATBit i
    | i == 7 = return True
    | i == 2 = do
        -- Compute the correct ly = lyc bit
        ly   <- readLY
        lyc  <- readLYC
        return $ ly == lyc 
    | i <= 1 = do
        -- Compute the current GPU mode
        cyc    <- askRef cycles
        lineS  <- askRef lineStart
        vbl    <- askRef vblankPeriod
        let diff = cyc - lineS
            (b1, b0) = case (vbl, diff >= 80, diff >= 248) of
                           (True, _, _)      -> (False, True)
                           (_, False, False) -> (True,  False)
                           (_, True, False)  -> (True,  True)
                           (_, True, True)   -> (False, False)
                           (_, False, True)  -> error "mathematically impossible"
        return $ if i == 0 then b0 else b1
    | otherwise = do
        stat <- askRef regSTAT
        return $ testBit stat i

readSTAT :: GBState s Word8
readSTAT = do
    stat <- askRef regSTAT
    let getBit x = do b <- readSTATBit x
                      return $ if b then bit x else 0
    [b7, b2, b1, b0] <- mapM getBit [7, 2, 1, 0]
    return $ stat .|. b7 .|. b2 .|. b1 .|. b0

writeSTAT :: Word8 -> GBState s ()
writeSTAT val = do
    old <- readSTAT
    putRef regSTAT $ val .&. 0x78

    -- Was mode 0 just requested? If so, make sure it does not trigger on this
    -- scanline if we are already past the trigger point
    let newBit3 = testBit val 3
        oldBit3 = testBit old 3
    when (newBit3 && not oldBit3) $ do
        line <- askRef lineStart
        cyc  <- askRef cycles
        let time = line + 248
        when (time <= cyc) $ putRef stat0Triggered True
        


---- Timer

[readTMA, readTAC] = map askRef [regTMA, regTAC]
[writeTMA] = map putRef [regTMA]

-- Keys

writeP1 = putRef regP1

quit :: GBState s ()
quit = do
    oldKeys <- askRef keys 
    let newKeys = Set.insert KeyQuit oldKeys
    putRef keys newKeys
         
