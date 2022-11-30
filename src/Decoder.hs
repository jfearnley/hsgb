module Decoder (runInstruction) where

import qualified Data.Vector as V
import Data.Word

import Types
import Cpu
import CpuBasic
import Util

runInstruction :: GBState s ()
runInstruction = do
    b1 <- pcPlus 0
    b2 <- pcPlus 1
    decode b1 b2


decode :: Word8 -> Word8 -> GBState s ()
decode 0xCB n = cbTable     `V.unsafeIndex` fromIntegral n
decode n    _ = decodeTable `V.unsafeIndex` fromIntegral n


decodeTable :: V.Vector (GBState s ())
decodeTable =
    let
        defaultInstr x = error $ "Failed to decode " ++ toHex (fromIntegral x)
        base = V.generate 256 defaultInstr
    in
        base V.// instructions

cbTable :: V.Vector (GBState s ())
cbTable =
    let
        defaultInstr x = error $ "Failed to decode CB " ++ toHex (fromIntegral x)
        base = V.generate 256 defaultInstr
    in
        base V.// cbInstructions

-- Argument lists for instructions

readArgs  = [readA,  readB,  readC,  readD,  readE,  readH,  readL]
writeArgs = [writeA, writeB, writeC, writeD, writeE, writeH, writeL]
regPairs  = zip readArgs writeArgs

read16Args  = [readBC,  readDE,  readHL,  readSP] 
write16Args = [writeBC, writeDE, writeHL, writeSP] 
regPairs16  = zip read16Args write16Args

arithOps = [add, adc, sub, sbc, andOp, orOp, xorOp, cp]
rotateOps = [rlc, rrc, rl, rr, sla, sra, swapOp, srl]
bitOps = map bitOp [0..7]
resOps = map res [0..7]
setOps = map set [0..7]

pushArgs = [readAF,  readBC,  readDE,  readHL]
popArgs  = [writeAF, writeBC, writeDE, writeHL]

flags = [always, nzCond, zCond, ncCond, cCond]

resetVecs = [0x0, 0x8, 0x10, 0x18, 0x20, 0x28, 0x30, 0x38]


-- Building a list of instructions from opcodes 

build :: [Int] -> [a] -> (a -> GBState s ()) -> [(Int, GBState s ())]
build codes args op = zip codes (map op args)

-- Instructions

-- XXX: STOP instruction is not implemented

instructions :: [(Int, GBState s ())]
instructions = 
    build [0x3E, 0x06, 0x0E, 0x16, 0x1E, 0x26, 0x2E] writeArgs loadImmediate ++
    build [0x7F, 0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D] readArgs (loadR2R writeA) ++
    build [0x47, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45] readArgs (loadR2R writeB) ++
    build [0x4F, 0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D] readArgs (loadR2R writeC) ++
    build [0x57, 0x50, 0x51, 0x52, 0x53, 0x54, 0x55] readArgs (loadR2R writeD) ++
    build [0x5F, 0x58, 0x59, 0x5A, 0x5B, 0x5C, 0x5D] readArgs (loadR2R writeE) ++
    build [0x67, 0x60, 0x61, 0x62, 0x63, 0x64, 0x65] readArgs (loadR2R writeH) ++
    build [0x6F, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D] readArgs (loadR2R writeL) ++
    build [0x01, 0x11, 0x21, 0x31] write16Args load16Immediate ++
    build [0x77, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75] readArgs hlWrite ++
    build [0x7E, 0x46, 0x4E, 0x56, 0x5E, 0x66, 0x6E] writeArgs hlRead ++ 
    build [0x87, 0x80, 0x81, 0x82, 0x83, 0x84, 0x85] readArgs add ++
    build [0x8F, 0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D] readArgs adc ++
    build [0x97, 0x90, 0x91, 0x92, 0x93, 0x94, 0x95] readArgs sub ++
    build [0x9F, 0x98, 0x99, 0x9A, 0x9B, 0x9C, 0x9D] readArgs sbc ++
    build [0xA7, 0xA0, 0xA1, 0xA2, 0xA3, 0xA4, 0xA5] readArgs andOp ++
    build [0xB7, 0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5] readArgs orOp  ++
    build [0xAF, 0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD] readArgs xorOp ++
    build [0xBF, 0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD] readArgs cp ++
    build [0x86, 0x8E, 0x96, 0x9E, 0xA6, 0xB6, 0xAE, 0xBE] arithOps arithHL ++
    build [0xC6, 0xCE, 0xD6, 0xDE, 0xE6, 0xF6, 0xEE, 0xFE] arithOps arithImm ++
    build [0x3C, 0x04, 0x0C, 0x14, 0x1C, 0x24, 0x2C] regPairs inc ++
    build [0x3D, 0x05, 0x0D, 0x15, 0x1D, 0x25, 0x2D] regPairs dec ++
    build [0x09, 0x19, 0x29, 0x39] read16Args add16 ++
    build [0x03, 0x13, 0x23, 0x33] regPairs16 inc16 ++
    build [0x0B, 0x1B, 0x2B, 0x3B] regPairs16 dec16 ++
    build [0xF5, 0xC5, 0xD5, 0xE5] pushArgs pushOp ++
    build [0xF1, 0xC1, 0xD1, 0xE1] popArgs popOp ++
    build [0xCD, 0xC4, 0xCC, 0xD4, 0xDC] flags callOp ++
    build [0xC9, 0xC0, 0xC8, 0xD0, 0xD8] flags retOp ++
    build [0xC7, 0xCF, 0xD7, 0xDF, 0xE7, 0xEF, 0xF7, 0xFF] resetVecs reset ++
    build [0x18, 0x20, 0x28, 0x30, 0x38] flags jrOp ++
    build [0xC3, 0xC2, 0xCA, 0xD2, 0xDA, 0xE9] flags jpOp ++
    [(0x00, noop), (0x02, wrBC), (0x07, rlca), (0x08, ldNNSP), (0x0A, reBC), 
     (0x0F, rrca),
     (0x12, wrDE), (0x17, rla), (0x1A, reDE), (0x1F, rra), 
     (0x22, writeHLI), (0x27, daa), (0x2A, readHLI), (0x2F, cpl),
     (0x32, writeHLD), (0x34, incHL), (0x35, decHL), (0x37, scf), (0x3A, readHLD),
     (0x36, ldHL), (0x3F, ccf),
     (0x76, halt),
     (0xD9, reti),
     (0xE0, wrIO), (0xE2, ldca), (0xE8, addSP), (0xE9, jpHL), (0xEA, wrNN), 
     (0xF0, reIO), (0xF2, ldac), (0xF3, di), (0xF8, ldhlSP), (0xF9, ldSPHL), 
     (0xFA, reNN), (0xFB, ei)] ++
    zip [0x10, 0xCB, 0xD3, 0xDB, 0xDD, 0xE3, 0xE4, 0xEB, 0xEC, 0xED, 0xF4, 0xFC, 0xFD] 
                                                                (repeat unimpl)


cbInstructions :: [(Int, GBState s ())]
cbInstructions = 
    build [0x1F, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D] regPairs rr ++
    build [0x0F, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D] regPairs rrc ++
    build [0x3F, 0x38, 0x39, 0x3A, 0x3B, 0x3C, 0x3D] regPairs srl ++
    build [0x2F, 0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D] regPairs sra ++ 
    build [0x17, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15] regPairs rl ++
    build [0x07, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05] regPairs rlc ++
    build [0x27, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25] regPairs sla ++
    build [0x37, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35] regPairs swapOp ++
    build [0x47, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45] readArgs (bitOp 0) ++
    build [0x4F, 0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D] readArgs (bitOp 1) ++
    build [0x57, 0x50, 0x51, 0x52, 0x53, 0x54, 0x55] readArgs (bitOp 2) ++
    build [0x5F, 0x58, 0x59, 0x5A, 0x5B, 0x5C, 0x5D] readArgs (bitOp 3) ++
    build [0x67, 0x60, 0x61, 0x62, 0x63, 0x64, 0x65] readArgs (bitOp 4) ++
    build [0x6F, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D] readArgs (bitOp 5) ++
    build [0x77, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75] readArgs (bitOp 6) ++
    build [0x7F, 0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D] readArgs (bitOp 7) ++
    build [0x87, 0x80, 0x81, 0x82, 0x83, 0x84, 0x85] regPairs (res 0) ++
    build [0x8F, 0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D] regPairs (res 1) ++
    build [0x97, 0x90, 0x91, 0x92, 0x93, 0x94, 0x95] regPairs (res 2) ++
    build [0x9F, 0x98, 0x99, 0x9A, 0x9B, 0x9C, 0x9D] regPairs (res 3) ++
    build [0xA7, 0xA0, 0xA1, 0xA2, 0xA3, 0xA4, 0xA5] regPairs (res 4) ++
    build [0xAF, 0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD] regPairs (res 5) ++
    build [0xB7, 0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5] regPairs (res 6) ++
    build [0xBF, 0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD] regPairs (res 7) ++
    build [0xC7, 0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5] regPairs (set 0) ++
    build [0xCF, 0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD] regPairs (set 1) ++
    build [0xD7, 0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5] regPairs (set 2) ++
    build [0xDF, 0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD] regPairs (set 3) ++
    build [0xE7, 0xE0, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5] regPairs (set 4) ++
    build [0xEF, 0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED] regPairs (set 5) ++
    build [0xF7, 0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5] regPairs (set 6) ++
    build [0xFF, 0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD] regPairs (set 7) ++
    build [0x06, 0x0E, 0x16, 0x1E, 0x26, 0x2E, 0x36, 0x3E] rotateOps bitHL ++
    build [0x46, 0x4E, 0x56, 0x5E, 0x66, 0x6E, 0x76, 0x7E] bitOps arithHL ++
    build [0x86, 0x8E, 0x96, 0x9E, 0xA6, 0xAE, 0xB6, 0xBE] resOps bitHL ++
    build [0xC6, 0xCE, 0xD6, 0xDE, 0xE6, 0xEE, 0xF6, 0xFE] setOps bitHL ++
    []
