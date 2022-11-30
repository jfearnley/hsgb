{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module GB where

import Data.Functor.Identity
import Data.Primitive.ByteArray
import qualified Data.Vector.Unboxed as VU
import qualified Data.ByteString as B
import qualified Data.Set as Set
import Control.Applicative
import Control.Monad.ST
import Control.Monad.RWS.Strict
import Data.Word
import Data.Char
import Data.Bits
import Data.STRef
import Data.List.Split
import Numeric
import System.Console.Pretty
import GHC.Generics

import Types
import Util
import Memory



---- Initialization

initGB :: PureGB
initGB = GB { 
              -- Registers
              regA  = Identity 0,
              regB  = Identity 0,
              regC  = Identity 0,
              regD  = Identity 0,
              regE  = Identity 0,
              regH  = Identity 0,
              regL  = Identity 0,

              regPC = Identity 0x100,
              regSP = Identity $ 0xFFFF - 1,

              -- Flags
              zero     = Identity False,
              negative = Identity False,
              hcarry   = Identity False,
              carry    = Identity False,

              -- Memory
              rom   = byteArrayFromList (replicate 0x8000 (0 :: Word8)),
              ram   = VU.replicate 0x2000 0,
              vram  = VU.replicate 0x2000 0,
              oam   = VU.replicate 0xA0 0,
              stack = VU.replicate 0x7F 0,
              cartRam = VU.empty,
              romBanks = NoRomBanks,
              ramSize  = NoRam,
              memController = Other,
              currentRomBank = Identity 1,
              currentRamBank = Identity 0,
              romRamMode = Identity RomBanking,
              ramEnable = Identity False,

              -- Interrupts
              ime   = Identity False,
              regIF = Identity 0xE0,
              regIE = Identity 0x00,
              enableEI = Identity Nothing,

              -- Timer
              lastDIVWrite = Identity 0,
              timerStarted = Identity 0,
              regTIMA = Identity 0,
              regTMA  = Identity 0,
              regTAC  = Identity 0xF8,

              -- GPU registers
              outputGraphics = Identity True,
              regLCDC = Identity 0x91,
              regSTAT = Identity 0,
              regSCY  = Identity 0,
              regSCX  = Identity 0,
              regBGP  = Identity 0xFC,
              regWX   = Identity 0,
              regWY   = Identity 0,
              regOBP0 = Identity 0xFF,
              regOBP1 = Identity 0xFF,
              regLYC  = Identity 0,
              windowLine = Identity 0,

              spriteCache = Identity [],
              spriteDirty = Identity True,

              -- Sound registers
              regNR10 = Identity 0,
              regNR11 = Identity 0,
              regNR12 = Identity 0,
              regNR13 = Identity 0,
              regNR14 = Identity 0,

              regNR21 = Identity 0,
              regNR22 = Identity 0,
              regNR23 = Identity 0,
              regNR24 = Identity 0,

              regNR30 = Identity 0,
              regNR31 = Identity 0,
              regNR32 = Identity 0,
              regNR33 = Identity 0,
              regNR34 = Identity 0,
              wavePattern = VU.replicate 32 0,

              regNR41 = Identity 0,
              regNR42 = Identity 0,
              regNR43 = Identity 0,
              regNR44 = Identity 0,

              regNR50 = Identity 0,
              regNR51 = Identity 0,
              regNR52 = Identity 0,

              -- Sound generation
              lastSound = Identity 0,
              sampleRemainder  = Identity 0,

              lastFS    = Identity 0,
              fsState   = Identity 0,

              ch1Playing      = Identity False,
              ch1Length       = Identity 0,
              ch1FreqIdx      = Identity 0,
              ch1Frequency    = Identity 0,
              ch1Volume       = Identity 0,
              ch1VolPeriod    = Identity 0,
              ch1VolCounter   = Identity 0, 
              ch1VolDirection = Identity 0,
              ch1Duty         = Identity 0, 
              ch1FreqSwpTime  = Identity 0,
              ch1FreqSwpDir   = Identity 0,
              ch1FreqSwpShift = Identity 0,
              ch1FreqSwpCntr  = Identity 1,

              ch2Playing      = Identity False,
              ch2Length       = Identity 0,
              ch2FreqIdx      = Identity 0,
              ch2Frequency    = Identity 0,
              ch2Volume       = Identity 0,
              ch2VolPeriod    = Identity 0,
              ch2VolCounter   = Identity 0, 
              ch2VolDirection = Identity 0,
              ch2Duty         = Identity 0, 
                
              chNPlaying      = Identity False,
              chNLength       = Identity 0,
              chNFreqIdx      = Identity 0,
              chNFrequency    = Identity 1,
              chNShortDuty    = Identity False,
              chNVolume       = Identity 0,
              chNVolPeriod    = Identity 0,
              chNVolCounter   = Identity 0, 
              chNVolDirection = Identity 0,

              chWPlaying      = Identity False,
              chWDACEnable    = Identity True,
              chWLength       = Identity 0,
              chWFreqIdx      = Identity 0,
              chWFrequency    = Identity 1,
              chWVolume       = Identity 1,
              chWVolCode      = Identity 0,
                
              -- IO
              serialChar   = Identity 0x0,
              serialBuffer = Identity "",

              -- Keys
              keys  = Identity Set.empty,
              regP1 = Identity 0,

              -- Event variables
              eventsStale = Identity False,
              nextEvent = Identity (Just 0), 
              vblankPeriod = Identity False,
              lineStart  = Identity 0,
              lineCount  = Identity 0,
              frameStart = Identity 0,
              frameCount = Identity 0,
              stat0Triggered = Identity False,
              stat2Triggered = Identity False,
              renderTriggered = Identity False,

              cycles = Identity 0
            }


---- Loading roms



mbcMap :: [([Word8], MemController)]
mbcMap = [
            ([0x00, 0x08, 0x09], RomOnly),
            ([0x01, 0x02, 0x03], MBC1),
            ([0x05, 0x06], MBC2),
            ([0x0F, 0x10, 0x11, 0x12, 0x013], MBC3),
            ([0x19, 0x1A, 0x1B, 0x1C, 0x01D, 0x1E], MBC5)
         ]

getMBC :: Word8 -> MemController
getMBC byte = case (filter (\ (x, _) -> byte `elem` x) mbcMap) of
                    []         -> Other
                    ((_, x):_) -> x

getRomBanks :: Word8 -> RomBanks
getRomBanks 0 = NoRomBanks
getRomBanks b = RomBanks (2^(b+1))

getRamSize :: Word8 -> RamSize
getRamSize 0x01 = TwoK
getRamSize 0x02 = RamBanks 1
getRamSize 0x03 = RamBanks 4
getRamSize 0x04 = RamBanks 16
getRamSize 0x05 = RamBanks 8
getRamSize _    = NoRam

ramLength :: RamSize -> Int
ramLength TwoK         = 0x800
ramLength (RamBanks i) = 0x2000 * i
ramLength NoRam        = 0

padRom :: [Word8] -> [Word8]
padRom rom = rom ++ padding
    where l       = 0x8000 - length (rom)
          padding = replicate l 0

loadRom :: String -> IO (Maybe PureGB)
loadRom file = do
    bytes <- B.readFile file 
    let gb      = initGB
        rom     = byteArrayFromList $ padRom (B.unpack bytes)
        mbc     = getMBC      $ indexByteArray rom 0x147
        romB    = getRomBanks $ indexByteArray rom 0x148
        ramS    = getRamSize  $ indexByteArray rom 0x149
        cartRam = VU.replicate (ramLength ramS) 0
        ret     = gb {
                        rom = rom, 
                        romBanks = romB,
                        ramSize  = ramS,
                        memController = mbc,
                        cartRam = cartRam
                     }
    return if mbc `elem` [RomOnly, MBC1] then Just ret
                                         else Nothing


getTitle :: PureGB -> String
getTitle gb = ascii
    where romv  = rom gb
          bytes = map (indexByteArray romv) [0x134..0x143] :: [Word8]
          ascii = map (chr . fromIntegral) bytes

instance Show RomBanks where
    show (NoRomBanks) = "No rom banks"
    show (RomBanks i) = show i ++ " rom banks"

instance Show RamSize where
    show (NoRam) = "No ram"
    show (TwoK) = "2kb of ram"
    show (RamBanks 1) = "1 ram bank"
    show (RamBanks i) = show i ++ " ram banks"

printRomStats :: PureGB -> IO ()
printRomStats gb = do
    putStrLn $ "Title:    " ++ getTitle gb
    putStrLn $ "Memory:   " ++ (show $ memController gb)
    putStrLn $ "RomBanks: " ++ (show $ romBanks gb)
    putStrLn $ "RamSize:  " ++ (show $ ramSize gb)



---- Converting between PureGB and STGB

class Transform i o a b c d where
    gtransform :: Monad m 
               => (forall x.               a x -> m (b x))
               -> (forall x. VU.Unbox x => c x -> m (d x))
               -> i p 
               -> m (o p)

-- Transform terms of type a to b
instance Transform (K1 m (a k)) (K1 m (b k)) a b c d where
    gtransform f _ (K1 z) = K1 <$> (f z)
    {-# INLINE gtransform #-}

-- Transform terms of type c to d
instance VU.Unbox k => Transform (K1 m (c k)) (K1 m (d k)) a b c d where
    gtransform _ g (K1 z) = K1 <$> (g z)
    {-# INLINE gtransform #-}

-- Transform terms that do not involve a or c
instance Transform (K1 m x) (K1 m x) a b c d where
    gtransform _ _ (K1 z) = return $ K1 z
    {-# INLINE gtransform #-}
    
instance (Transform i o a b c d, Transform i' o' a b c d)
       => Transform (i :*: i') (o :*: o') a b c d where
    gtransform f g (l :*: r) = 
        liftA2 (:*:) (gtransform f g l) (gtransform f g r)
    {-# INLINE gtransform #-}
    
instance (Transform i o a b c d, Transform i' o' a b c d)
       => Transform (i :+: i') (o :+: o') a b c d where
    gtransform f g (L1 l) = L1 <$> gtransform f g l
    gtransform f g (R1 r) = R1 <$> gtransform f g r
    {-# INLINE gtransform #-}
  
instance Transform i o a b c d
      => Transform (M1 _a _b i) (M1 _a' _b' o) a b c d where
    gtransform f g (M1 x) = M1 <$> gtransform f g x
    {-# INLINE gtransform #-}


transform :: ( Generic (f a c)
             , Generic (f b d)
             , Transform (Rep (f a c)) (Rep (f b d)) a b c d
             )
          => (forall x.               a x -> m (b x))
          -> (forall x. VU.Unbox x => c x -> m (d x))
          -> f a c
          -> Monad m => m (f b d)
transform f g = fmap to . (gtransform f g) . from

idToSt :: Identity a -> ST s (STRef s a)
idToSt = newSTRef . runIdentity 

stToId :: STRef s b -> ST s (Identity b)
stToId = (fmap Identity) . readSTRef 

toST   :: PureGB -> ST s (STGB s)
toST   = transform idToSt VU.thaw   

fromST :: STGB s -> ST s (PureGB)
fromST = transform stToId VU.freeze 

---- Running the GBState monad


runState :: (forall s. GBState s a) -> PureGB -> (a, PureGB, EmulatorOutput)
runState op gb = runST $
    do 
        stgb <- toST gb
        (val, output, _) <- runRWST op stgb mempty
        gb' <- fromST stgb
        return (val, gb', output)

evalState :: (forall s. GBState s a) -> PureGB -> a
evalState op gb = x
    where (x, _, _) = runState op gb

execState :: (forall s. GBState s ()) -> PureGB -> PureGB
execState op gb = x
    where (_, x, _) = runState op gb



---- Getting registers from a PureGB

askPure :: (PureGB -> Identity a) -> PureGB -> a
askPure f = runIdentity . f

[regAPure, regBPure, regCPure, regDPure, regEPure, regHPure, regLPure] = 
    map (runIdentity .) [regA, regB, regC, regD, regE, regH, regL]

[zeroPure, negativePure, hcarryPure, carryPure] = 
    map (runIdentity .) [zero, negative, hcarry, carry] 

regFPure :: PureGB -> Word8
regFPure gb = ind zeroPure * 128 + ind negativePure * 64 + ind hcarryPure * 32 + ind carryPure * 16
    where ind x = if x gb then 1 else 0

regAFPure :: PureGB -> Word16
regAFPure gb = to16 (regFPure gb) (regAPure gb)

regBCPure :: PureGB -> Word16
regBCPure gb = to16 (regCPure gb) (regBPure gb)

regDEPure :: PureGB -> Word16
regDEPure gb = to16 (regEPure gb) (regDPure gb)

regHLPure :: PureGB -> Word16 
regHLPure gb = to16 (regLPure gb) (regHPure gb)


---- Show functions

showf gb to_str (str, f) = (style Bold str) ++ ": "  ++ (to_str . f $ gb) ++ " "
linef gb to_str = unwords . map (showf gb to_str) 
bitf gb x reg = concatMap (boolToInt . testBit (reg gb)) [x,(x-1)..0]

boolToInt True  = "1"
boolToInt False = "0"


showMemory :: PureGB -> (PureGB -> Word16) -> String
showMemory gb reg = 
    let
        reg_line = fromIntegral (reg gb `div` 16) :: Int
        lower = fromIntegral $ max (reg_line-1) 0 
        upper = fromIntegral $ min (reg_line+5) (0xFFFF `div` 16+1) 

        readMemAddr x = evalState (readMem x) gb
        elements = map readMemAddr [lower*16..upper*16-1]
        hexed   = map toHex elements
        chunked = map unwords $ chunksOf 16 hexed
        with_lines = zipWith (\ x y -> toHex16 (16*x) ++ ":  " ++ y) [fromIntegral lower.. fromIntegral upper] chunked
    in
        "        0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F\n" ++
        unlines with_lines 


showRegs :: PureGB -> String
showRegs gb =
    let
        -- Registers
        regs8  = [("A", regAPure), ("B", regBPure), ("C", regCPure), ("D", regDPure), ("E", regEPure), ("H", regHPure), ("L", regLPure)]
        regs16 = [("PC", askPure regPC), ("SP", askPure regSP), ("AF", regAFPure), ("BC", regBCPure), ("DE", regDEPure), ("HL", regHLPure)]

        regs8str  = linef gb show regs8
        regs16str = linef gb toHex16 regs16
    in
        unlines [regs8str, regs16str]
        

showFlags :: PureGB -> String
showFlags gb =
    let
        flags = [("Z", zeroPure),("N", negativePure), ("H", hcarryPure), ("C", carryPure)]
    in
        linef gb boolToInt flags ++ "\n"


showInterrupts :: PureGB -> String
showInterrupts gb =  "IE: " ++ bitf gb 4 (askPure regIE) ++ 
                    " IF: " ++ bitf gb 4 (askPure regIF) ++ 
                    " IME: " ++ boolToInt (askPure ime gb) ++ "\n"

showGPU :: PureGB -> String
showGPU gb =  "LCDC: " ++ bitf gb 7 (askPure regLCDC) ++ 
             " STAT: " ++ bitf gb 7 (askPure regSTAT) ++ "\n" ++
               linef gb show [(" SCY", (askPure regSCY)), 
                              ("SCX", (askPure regSCX)), 
                              ("BGP", (askPure regBGP)), 
                              ("WY", (askPure regWY)), 
                              ("WX", (askPure regWX)), 
                              ("LYC", (askPure regLYC))] ++ "\n"

instance Show PureGB where
    show gb = 
        let
            -- Misc
            miscstr = showf gb show ("Cycles", (askPure cycles)) ++ " (" ++ showFFloat (Just 2) (fromIntegral (askPure cycles gb) / (1024*4096) :: Double) "" ++ " seconds)"

            -- Memory
            pc_mem_str = showMemory gb (askPure regPC)
            sp_mem_str = showMemory gb (askPure regSP)
            pureRegHL  = \gb -> Identity $ to16 (runIdentity $ regL gb) (runIdentity $ regH gb)
            hl_mem_str = showMemory gb (askPure pureRegHL)
        in
            showRegs gb ++ showFlags gb ++ showInterrupts gb ++ showGPU gb ++
            unlines [miscstr, "", pc_mem_str, hl_mem_str, sp_mem_str]
