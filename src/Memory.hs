module Memory where

import Data.Bits
import Data.Word
import Debug.Trace
import Control.Monad.Reader
import Data.Primitive.ByteArray
import qualified Data.Vector.Unboxed.Mutable as VM

import Types
import Util
import CpuBasic
import Timer
import IO
import Sound

---- Rom banking

readRomVec :: Int -> GBState s Word8
readRomVec idx = do
    rom <- asks rom
    return $ indexByteArray rom idx

{-# INLINE readRomVec #-}


romIdx :: Int -> Int -> Int
romIdx addr bank = bank * 0x4000 + (addr - 0x4000)

getRomBank :: GBState s Int
getRomBank = do
    rombank <- askRef currentRomBank
    rambank <- askRef currentRamBank
    mode    <- askRef romRamMode
    return $ case mode of
                RomBanking -> 32 * rambank + rombank
                RamBanking -> rombank


writeRomBank :: Word8 -> GBState s ()
writeRomBank val = putRef currentRomBank x
    where bank = fromIntegral $ val .&. 0x1F
          x    = case bank of
                    0x00 -> 0x01
                    _    -> bank
    

writeRamBank :: Word8 -> GBState s ()
writeRamBank val = putRef currentRamBank x
    where x = fromIntegral $ val .&. 0x03

writeRomRamSelect :: Word8 -> GBState s ()
writeRomRamSelect val = 
    putRef romRamMode $ case (val .&. 0x01) of
                                    0x00 -> RomBanking
                                    0x01 -> RamBanking
                                    _    -> error "impossible"



readRom :: Int -> GBState s Word8
readRom addr
    | addr < 0x4000 = readRomVec addr -- Read bank 0
    | otherwise = do
                    bank <- getRomBank
                    readRomVec $ romIdx addr bank
{-# INLINE readRom #-}

---- External Ram banking

getRamBank :: GBState s Int
getRamBank = do
    rambank <- askRef currentRamBank
    mode    <- askRef romRamMode
    return $ case mode of
                RomBanking -> 0
                RamBanking -> rambank


ramIdx :: Int -> Int -> Int
ramIdx addr bank = bank * 0x2000 + addr 

readCartRam :: Int -> GBState s Word8
readCartRam addr = do
    bank <- getRamBank
    cram  <- asks cartRam
    enabled <- askRef ramEnable
    let realAddr = ramIdx addr bank
        goodAddr = realAddr < VM.length cram
    case (enabled, goodAddr) of
        (True, True) -> VM.read cram realAddr
        _            -> return 0xFF


writeCartRam :: Word16 -> Word8 -> GBState s ()
writeCartRam addr val = do
    bank <- getRamBank
    cram  <- asks cartRam
    enabled <- askRef ramEnable
    let realAddr = ramIdx (fromIntegral addr) bank
        goodAddr = realAddr < VM.length cram
    case (enabled, goodAddr) of
        (True, True) -> VM.write cram realAddr val
        _            -> return ()


---- Ram enable

setRamEnable :: Word8 -> GBState s ()
setRamEnable val = putRef ramEnable (x == 0x0A)
    where x = val .&. 0x0F

---- Memory reads

readVec :: (STGB s -> VM.MVector s Word8) -> Int -> Read8 s
readVec vec addr = do
    v <- asks vec
    lift $ VM.read v addr


[readVRAM, readRAM, readOAM, readStack] 
    = map readVec [vram, ram, oam, stack]

readWavePattern :: Word16 -> GBState s Word8 
readWavePattern addr = do
    let baseAddr = fromIntegral $ addr * 2
    high <- readVec wavePattern baseAddr
    low  <- readVec wavePattern (baseAddr + 1)
    return $ shiftL high 4 .|. low


readMem :: Word16 -> GBState s Word8
readMem addr  
    | addr < 0x8000 = readRom iaddr
    | addr < 0xA000 = readVRAM (iaddr - 0x8000)
    | addr < 0xC000 = readCartRam (iaddr - 0xA000)
    | addr < 0xE000 = readRAM (iaddr - 0xC000)
    | addr < 0xFE00 = readRAM (iaddr - 0xE000)
    | addr < 0xFEA0 = readOAM (iaddr - 0xFE00)
    | addr < 0xFF00 = return 0xFF
    | addr < 0xFF4C = readMemIO addr
    | addr < 0xFF80 = return 0xFF
    | addr < 0xFFFF = readStack (iaddr - 0xFF80)
    | otherwise     = readIE
    where iaddr = fromIntegral addr

readMemIO :: Word16 -> GBState s Word8
readMemIO addr 
    | addr == 0xFF00 = readP1
    | addr == 0xFF04 = readDIV
    | addr == 0xFF05 = readTIMA
    | addr == 0xFF06 = readTMA
    | addr == 0xFF07 = readTAC
    | addr == 0xFF0F = readIF 
    | addr == 0xFF10 = readNR10
    | addr == 0xFF11 = readNR11
    | addr == 0xFF12 = readNR12
    | addr == 0xFF13 = readNR13
    | addr == 0xFF14 = readNR14
    | addr == 0xFF16 = readNR21
    | addr == 0xFF17 = readNR22
    | addr == 0xFF18 = readNR23
    | addr == 0xFF19 = readNR24
    | addr == 0xFF1A = readNR30
    | addr == 0xFF1B = readNR31
    | addr == 0xFF1C = readNR32
    | addr == 0xFF1D = readNR33
    | addr == 0xFF1E = readNR34
    | addr == 0xFF20 = readNR41
    | addr == 0xFF21 = readNR42
    | addr == 0xFF22 = readNR43
    | addr == 0xFF23 = readNR44
    | addr == 0xFF24 = readNR50
    | addr == 0xFF25 = readNR51
    | addr == 0xFF26 = readNR52
    | 0xFF30 <= addr && addr <= 0xFF3F = readWavePattern (fromIntegral addr - 0xFF30)
    | addr == 0xFF40 = readLCDC
    | addr == 0xFF41 = readSTAT
    | addr == 0xFF42 = readSCY
    | addr == 0xFF43 = readSCX
    | addr == 0xFF44 = readLY
    | addr == 0xFF45 = readLYC
    | addr == 0xFF47 = readBGP
    | addr == 0xFF48 = readOBP0
    | addr == 0xFF49 = readOBP1
    | addr == 0xFF4A = readWY
    | addr == 0xFF4B = readWX
    | otherwise =  return 0xFF
    | otherwise =  trace ("Unhandled read from " ++ toHex16 addr) $ return 0xFF


---- Memory writes

writeVec :: (STGB s -> VM.MVector s Word8) -> Word16 -> Write8 s
writeVec vec addr val = do
    v <- asks vec
    lift $ VM.write v (fromIntegral addr) val

[writeRam, writeVRam, writeStack] = 
    map writeVec [ram, vram, stack]

writeOAM :: Word16 -> Write8 s
writeOAM addr val = do
    putRef spriteDirty True 
    writeVec oam addr val

writeDMA :: Word8 -> GBState s ()
writeDMA val = 
    let
        val16 = fromIntegral val :: Word16
        faddr = 256*val16
        taddr = 0xFE00
        copy i = readMem (faddr + i) >>= writeMem (taddr + i)
    in
        sequence_ [copy i | i <- [0..0x9F]]

writeWavePattern :: Word16 -> Word8 -> GBState s ()
writeWavePattern addr val = do
    let baseAddr = addr * 2
    writeVec wavePattern baseAddr       $ shiftR (val .&. 0xF0) 4
    writeVec wavePattern (baseAddr + 1) $ val .&. 0x0F

writeIO :: Word16 -> Word8 -> GBState s ()
writeIO addr 
    | addr == 0xFF00 = writeP1
    | addr == 0xFF01 = writeSerial
    | addr == 0xFF02 = writeSerialC
    | addr == 0xFF04 = markEvents writeDIV
    | addr == 0xFF05 = markEvents writeTIMA
    | addr == 0xFF06 = markEvents writeTMA
    | addr == 0xFF07 = markEvents writeTAC
    | addr == 0xFF0F = markEvents writeIF 
    | addr == 0xFF10 = writeNR10
    | addr == 0xFF11 = writeNR11
    | addr == 0xFF12 = writeNR12
    | addr == 0xFF13 = writeNR13
    | addr == 0xFF14 = writeNR14
    | addr == 0xFF16 = writeNR21
    | addr == 0xFF17 = writeNR22
    | addr == 0xFF18 = writeNR23
    | addr == 0xFF19 = writeNR24
    | addr == 0xFF1A = writeNR30
    | addr == 0xFF1B = writeNR31
    | addr == 0xFF1C = writeNR32
    | addr == 0xFF1D = writeNR33
    | addr == 0xFF1E = writeNR34
    | addr == 0xFF20 = writeNR41
    | addr == 0xFF21 = writeNR42
    | addr == 0xFF22 = writeNR43
    | addr == 0xFF23 = writeNR44
    | addr == 0xFF24 = writeNR50
    | addr == 0xFF25 = writeNR51
    | addr == 0xFF26 = writeNR52
    | 0xFF30 <= addr && addr <= 0xFF3F = writeWavePattern (addr - 0xFF30)
    | addr == 0xFF40 = markEvents writeLCDC 
    | addr == 0xFF41 = markEvents writeSTAT
    | addr == 0xFF42 = writeSCY
    | addr == 0xFF43 = writeSCX
    | addr == 0xFF45 = markEvents writeLYC
    | addr == 0xFF46 = writeDMA
    | addr == 0xFF47 = writeBGP
    | addr == 0xFF48 = writeOBP0
    | addr == 0xFF49 = writeOBP1
    | addr == 0xFF4A = writeWY 
    | addr == 0xFF4B = writeWX 
    | otherwise = const $ return ()
    where markEvents act = \x -> markEventsStale >> act x

writeMem :: Word16 -> Word8 -> GBState s ()
writeMem addr val 
    | addr < 0x2000 = setRamEnable val
    | addr < 0x4000 = writeRomBank val
    | addr < 0x6000 = writeRamBank val
    | addr < 0x8000 = writeRomRamSelect val
    | addr < 0xA000 = writeVRam (addr - 0x8000) val
    | addr < 0xC000 = writeCartRam (addr - 0xA000) val
    | addr < 0xE000 = writeRam (addr - 0xC000) val
    | addr < 0xFE00 = writeRam (addr - 0xE000) val
    | addr < 0xFEA0 = writeOAM (addr - 0xFE00) val
    | addr < 0xFF00 = return ()
    | addr < 0xFF4C = writeIO addr val 
    | addr < 0xFF80 = return ()
    | addr < 0xFFFF = writeStack (addr - 0xFF80) val
    | otherwise     = markEventsStale >> writeIE val
    

writeMem16 :: Word16 -> Word16 -> GBState s ()
writeMem16 addr val = writeMem addr low >> writeMem (addr+1) high
    where (low, high) = from16 val
