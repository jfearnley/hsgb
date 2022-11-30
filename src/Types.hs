module Types where 

import Data.Word
import Data.Ratio
import Data.Functor.Identity
import Data.Monoid.Generic
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Primitive.ByteArray
import qualified Data.Set as Set
import Control.Monad.RWS.Strict
import Control.Monad.ST
import Data.STRef
import Data.DList 
import Data.IORef
import Data.Store
import SDL
import Foreign.Ptr
import Data.HashTable.IO
import GHC.Generics


-- The main gameboy type

data GB c  -- Container type for single variables 
        vu -- Unboxed vector type
        = GB {
               regA :: c Word8,
               regB :: c Word8,
               regC :: c Word8,
               regD :: c Word8,
               regE :: c Word8,
               regH :: c Word8,
               regL :: c Word8,

               regPC :: c Word16,
               regSP :: c Word16,

               -- Flags
               zero     :: c Bool,
               negative :: c Bool,
               hcarry   :: c Bool,
               carry    :: c Bool,

               -- Memory
               rom   :: ByteArray,
               ram   :: vu Word8,
               vram  :: vu Word8,
               oam   :: vu Word8,
               stack :: vu Word8,
               cartRam :: vu Word8,

               -- Memory Controller
               romBanks :: RomBanks,
               ramSize  :: RamSize,
               memController :: MemController,
               currentRomBank :: c Int,
               currentRamBank :: c Int,
               romRamMode :: c RomRamMode,
               ramEnable :: c Bool,

               -- Interrupts
               ime   :: c Bool,
               regIF :: c Word8,
               regIE :: c Word8,
               enableEI :: c (Maybe Integer),

               -- Timer
               lastDIVWrite :: c Integer,
               timerStarted :: c Integer,
               regTIMA :: c Word8,
               regTMA  :: c Word8,
               regTAC  :: c Word8,

               -- GPU registers
               outputGraphics :: c Bool,
               regLCDC :: c Word8,
               regSTAT :: c Word8,
               regSCY  :: c Word8,
               regSCX  :: c Word8,
               regBGP  :: c Word8,
               regWX   :: c Word8,
               regWY   :: c Word8,
               regOBP0 :: c Word8,
               regOBP1 :: c Word8,
               regLYC  :: c Word8,
               windowLine :: c Word8,

               spriteCache :: c [Sprite],
               spriteDirty :: c Bool,

               -- Sound registers
               regNR10 :: c Word8,
               regNR11 :: c Word8,
               regNR12 :: c Word8,
               regNR13 :: c Word8,
               regNR14 :: c Word8,

               regNR21 :: c Word8,
               regNR22 :: c Word8,
               regNR23 :: c Word8,
               regNR24 :: c Word8,

               regNR30 :: c Word8,
               regNR31 :: c Word8,
               regNR32 :: c Word8,
               regNR33 :: c Word8,
               regNR34 :: c Word8,
               wavePattern :: vu Word8,

               regNR41 :: c Word8,
               regNR42 :: c Word8,
               regNR43 :: c Word8,
               regNR44 :: c Word8,

               regNR50 :: c Word8,
               regNR51 :: c Word8,
               regNR52 :: c Word8,

               -- Sound generation
               lastSound :: c Integer,
               sampleRemainder :: c Frac,

               lastFS    :: c Integer,
               fsState   :: c Integer,

               ch1Playing      :: c Bool,
               ch1Length       :: c Int,
               ch1FreqIdx      :: c Frac,
               ch1Frequency    :: c Int,
               ch1Volume       :: c Int,
               ch1VolPeriod    :: c Int,
               ch1VolCounter   :: c Int,
               ch1VolDirection :: c Int,
               ch1Duty         :: c Int,
               ch1FreqSwpTime  :: c Int,
               ch1FreqSwpDir   :: c Int,
               ch1FreqSwpShift :: c Int,
               ch1FreqSwpCntr  :: c Int,

               ch2Playing      :: c Bool,
               ch2Length       :: c Int,
               ch2FreqIdx      :: c Frac,
               ch2Frequency    :: c Int,
               ch2Volume       :: c Int,
               ch2VolPeriod    :: c Int,
               ch2VolCounter   :: c Int,
               ch2VolDirection :: c Int,
               ch2Duty         :: c Int,

               chNPlaying      :: c Bool,
               chNLength       :: c Int,
               chNFreqIdx      :: c Frac,
               chNFrequency    :: c Int,
               chNShortDuty    :: c Bool,
               chNVolume       :: c Int,
               chNVolPeriod    :: c Int,
               chNVolCounter   :: c Int,
               chNVolDirection :: c Int,

               chWPlaying      :: c Bool,
               chWDACEnable    :: c Bool,
               chWLength       :: c Int,
               chWFreqIdx      :: c Frac,
               chWFrequency    :: c Int,
               chWVolume       :: c Int,
               chWVolCode      :: c Int,

               -- Serial port
               serialChar :: c Word8,
               serialBuffer :: c String,

               -- Keys
               keys :: c KeySet,
               regP1 :: c Word8,

               -- Event variables
               eventsStale :: c Bool,
               nextEvent :: c (Maybe Integer),
               vblankPeriod :: c Bool,
               lineStart  :: c Integer,
               lineCount  :: c Int,
               frameStart :: c Integer, 
               frameCount :: c Integer,
               stat0Triggered :: c Bool,
               stat2Triggered :: c Bool,
               renderTriggered :: c Bool,

               cycles :: c Integer
             } deriving (Generic)

-- instance Store (GB Identity VU.Vector)

type PureGB = GB Identity   VU.Vector      
type STGB s = GB (STRef s) (VUM.MVector s) 


type GBState s a = RWST (STGB s) () EmulatorOutput (ST s) a


-- Emulator output
data EmulatorOutput = EmulatorOutput 
                      {
                          belowBg    :: DList RenderInstruction,
                          background :: DList RenderInstruction,
                          sprites    :: DList RenderInstruction,

                          soundL     :: DList Samples,
                          soundR     :: DList Samples
                      } 
                      deriving Generic
                      deriving Semigroup via GenericSemigroup EmulatorOutput
                      deriving Monoid    via GenericMonoid    EmulatorOutput


-- Cpu types
type Read8 s  = GBState s Word8
type Read16 s = GBState s Word16

type Write8 s  = Word8  -> GBState s ()
type Write16 s = Word16 -> GBState s ()


-- Memory types

data MemController = RomOnly | MBC1 | MBC2 | MBC3 | MBC5 | Other
                                                            deriving (Show, Eq)

data RomBanks = NoRomBanks | RomBanks Int 
data RamSize  = NoRam | TwoK | RamBanks Int
data RomRamMode = RomBanking | RamBanking deriving Show


-- Gpu types

data Sprite = Sprite {
    ypos :: Int,
    xpos :: Int, 
    tile :: Word8,
    sflags :: Word8
} deriving Show

data RenderType = RendSprite | RendBackground deriving (Show, Eq)

data RenderInstruction = RenderInstruction {
    renderType :: RenderType,
    byte1      :: Word8,
    byte2      :: Word8,
    palette    :: Word8,
    toX        :: Int,
    toY        :: Int,
    rightClip  :: Maybe Int
}

-- Sound types

data Channel = Channel1 | Channel2 | Wave | Noise deriving (Show, Eq)

type Frac = Ratio Int
type Samples = VU.Vector Word8
type SampleList = DList Samples

-- Frontend types

type OurTexture = Ptr ()

type HashTable k v = Data.HashTable.IO.BasicHashTable k v

data Frontend = Frontend {
    renderer :: Renderer,
    window   :: Window,

    textureCache  :: HashTable (Bool, Word8, Word8, Word8) (VU.Vector Word8),
    textureCache2 :: HashTable (Bool, Word8, Word8, Word8) (VU.Vector Word8),

    audioDevice  :: AudioDevice,
    audioBufferL :: IORef (DList Samples),
    audioBufferR :: IORef (DList Samples),

    feTimer  :: Double,
    initTime :: Word32,

    cpuDelayTime :: IORef Int
}
        

data Key = KeyUp 
         | KeyDown 
         | KeyLeft 
         | KeyRight 
         | KeyA 
         | KeyB 
         | KeyStart 
         | KeySelect 
         | KeyQuit
         | KeyDebug
         deriving (Show, Read, Ord, Eq, Bounded, Enum)
type KeySet = Set.Set Key



