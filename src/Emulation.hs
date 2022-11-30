module Emulation where

import Control.Monad
import Control.Monad.Loops
import Data.Functor.Identity

import GB
import CpuBasic
import Types
import Decoder
import Interrupts

step :: GBState s ()
step = do
    interrupts
    runInstruction

nextVBlank :: GBState s ()
nextVBlank = do
    whileM_ (askRef vblankPeriod) step
    untilM_ step (askRef vblankPeriod)


nStep :: Int -> GBState s ()
nStep n = replicateM_ n step

nFrame :: Int -> GBState s ()
nFrame n = replicateM_ n nextVBlank

emulateFrame :: PureGB -> (PureGB, EmulatorOutput)
emulateFrame gb = (gb', out)
    where (_, gb', out) = runState (nFrame 1) gb

emulateFrameSkip :: PureGB -> (PureGB, EmulatorOutput)
emulateFrameSkip gb = (gb3, out)
    where gb1        = gb { outputGraphics = Identity False }
          (gb2, out) = emulateFrame gb1
          gb3        = gb2 { outputGraphics = Identity True }

emulate :: PureGB -> Int -> (PureGB, EmulatorOutput)
emulate gb 0 = emulateFrame gb
emulate gb n = let (gb1, out1) = emulateFrameSkip gb
                   (gb2, out2) = emulate gb1 (n-1)
               in (gb2, out1 <> out2)

