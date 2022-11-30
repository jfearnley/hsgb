module Disassembler (decodeGB, decodeGBSymb, Instruction, applySymbols, unconditionalBranch, showInstr) where

import Data.Word
import Data.Int
import qualified Data.Map.Strict as Map
import qualified Data.Bimap as Bimap
import System.Console.Pretty

import Util
import Types (PureGB)
import GB
import Memory

-- Instruction types

data Argument = 
      RegA
    | RegB
    | RegC
    | RegD
    | RegE
    | RegH
    | RegL
    | RegAF
    | RegBC
    | RegDE
    | RegHL
    | RegHLInc
    | RegHLDec
    | RegSP
    | RegSPPlus Int8
    | RegPC
    | Data8  Word8
    | Data16 (Either Word16 String)
    | Signed8 Int8
    | Address Argument
    | PlusC
    | Const Int

instance Show Argument where
    show RegA = "a"
    show RegB = "b"
    show RegC = "c"
    show RegD = "d"
    show RegE = "e"
    show RegH = "h"
    show RegL = "l"
    show RegAF = "af"
    show RegBC = "bc"
    show RegDE = "de"
    show RegHL = "hl"
    show RegHLInc = "hl+"
    show RegHLDec = "hl-"
    show RegSP = "sp"
    show (RegSPPlus x) = "sp" ++ (if x < 0 then "" else "+") ++ show x 
    show RegPC = "pc"
    show (Data8 x)  = "$" ++ toHex x
    show (Data16 (Left x)) = "$" ++ toHex16 x
    show (Data16 (Right x)) = x
    show (Signed8 x) = show x
    show (Address x) = "[" ++ show x ++ "]"
    show PlusC = "$FF00+c"
    show (Const x) = show x

data Condition = 
      Unconditional
    | Zero
    | NonZero
    | Carry
    | NoCarry

instance Show Condition where
    show Unconditional = ""
    show Zero     = "z"
    show NonZero  = "nz"
    show Carry    = "c"
    show NoCarry  = "nc"

aHL = Address RegHL

data Instruction = 
      ZeroArg String 
    | OneArg String Argument 
    | TwoArg String Argument Argument
    | BranchOne String Condition 
    | BranchTwo String Condition Argument
    | Undefined

showOp :: String -> String
showOp op = color Cyan op ++ pad
    where pad = replicate (4 - length op) ' '

showInstr :: (String -> String) -> Instruction -> String
showInstr opf (ZeroArg op)    = opf op
showInstr opf (OneArg op x)   = opf op ++ " " ++ show x 
showInstr opf (TwoArg op x y) = opf op ++ " " ++ show x ++ "," ++ show y
showInstr opf (BranchOne op x) = opf op ++ " " ++ show x 
showInstr opf (BranchTwo op x y) = opf op ++ " " ++ c ++ (if null c then "" else ",") ++ show y
    where c = show x
showInstr _ Undefined = "undefined"

instance Show Instruction where
    show = showInstr showOp


unconditionalBranch :: Instruction -> Bool
unconditionalBranch (BranchOne _ Unconditional)   = True
unconditionalBranch (BranchTwo _ Unconditional _) = True
unconditionalBranch _ = False

-- Instruction decoding

type InstrInfo = (Word8 -> Word8 -> Instruction, Int)


ldimm :: String -> Argument -> InstrInfo
ldimm op dest = (\x _ -> TwoArg op dest (Data8 x), 2)

ldimm16 :: String -> Argument -> InstrInfo
ldimm16 op dest = (\x y -> TwoArg op dest (Data16 $ Left $ to16 x y), 3)

imm :: String -> InstrInfo
imm op = (\x _ -> OneArg op (Data8 x), 2)

zero :: String -> InstrInfo
zero op = (\_ _ -> ZeroArg op, 1)

one :: String -> Argument -> InstrInfo
one op src = (\_ _ -> OneArg op src, 1)

two :: String -> Argument -> Argument -> InstrInfo
two op dest src = (\_ _ -> TwoArg op dest src, 1)

branch8 :: String -> Condition -> InstrInfo
branch8 op cond = (\x _ -> BranchTwo op cond $ Data8 $ x, 2)

branch16 :: String -> Condition -> InstrInfo
branch16 op cond = (\x y -> BranchTwo op cond $ Data16 $ Left $ to16 x y, 3)

ret :: Condition -> InstrInfo
ret cond = (\_ _ -> BranchOne "ret" cond, 1)


ldh :: (Argument -> Instruction) -> InstrInfo
ldh constructor = (\x _ -> constructor $ Address (Data16 $ Left $ 0xFF00 + fromIntegral x), 2)


regArgs   = [RegA, RegB, RegC, RegD, RegE, RegH, RegL]
reg16Args = [RegBC, RegDE, RegHL, RegSP] 
pushArgs = [RegAF,  RegBC,  RegDE,  RegHL]
arithOps  = ["add", "adc", "sub", "sbc", "and", "or", "xor", "cp"]
flags = [Unconditional, NonZero, Zero, NoCarry, Carry]
resetVecs = map Data8 [0x0, 0x8, 0x10, 0x18, 0x20, 0x28, 0x30, 0x38]

build :: [a] -> [b] -> (b -> c) -> [(a, c)]
build codes args op = zip codes (map op args)

instructions :: [(Word8, InstrInfo)]
instructions = 
    build [0x3E, 0x06, 0x0E, 0x16, 0x1E, 0x26, 0x2E] regArgs (ldimm "ld") ++
    build [0x7F, 0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D] regArgs (two "ld" RegA) ++
    build [0x47, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45] regArgs (two "ld" RegB) ++
    build [0x4F, 0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D] regArgs (two "ld" RegC) ++
    build [0x57, 0x50, 0x51, 0x52, 0x53, 0x54, 0x55] regArgs (two "ld" RegD) ++
    build [0x5F, 0x58, 0x59, 0x5A, 0x5B, 0x5C, 0x5D] regArgs (two "ld" RegE) ++
    build [0x67, 0x60, 0x61, 0x62, 0x63, 0x64, 0x65] regArgs (two "ld" RegH) ++
    build [0x6F, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D] regArgs (two "ld" RegL) ++
    build [0x01, 0x11, 0x21, 0x31] reg16Args (ldimm16 "ld") ++
    build [0x77, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75] regArgs (two "ld" aHL) ++
    build [0x7E, 0x46, 0x4E, 0x56, 0x5E, 0x66, 0x6E] regArgs (flip (two "ld") aHL) ++ 
    build [0x87, 0x80, 0x81, 0x82, 0x83, 0x84, 0x85] regArgs (one "add") ++
    build [0x8F, 0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D] regArgs (one "adc") ++
    build [0x97, 0x90, 0x91, 0x92, 0x93, 0x94, 0x95] regArgs (one "sub") ++
    build [0x9F, 0x98, 0x99, 0x9A, 0x9B, 0x9C, 0x9D] regArgs (one "sbc") ++
    build [0xA7, 0xA0, 0xA1, 0xA2, 0xA3, 0xA4, 0xA5] regArgs (one "and") ++
    build [0xB7, 0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5] regArgs (one "or")  ++
    build [0xAF, 0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD] regArgs (one "xor") ++
    build [0xBF, 0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD] regArgs (one "cp")  ++
    build [0x86, 0x8E, 0x96, 0x9E, 0xA6, 0xB6, 0xAE, 0xBE] arithOps (flip one aHL) ++
    build [0xC6, 0xCE, 0xD6, 0xDE, 0xE6, 0xF6, 0xEE, 0xFE] arithOps imm ++
    build [0x3C, 0x04, 0x0C, 0x14, 0x1C, 0x24, 0x2C] regArgs (one "inc") ++
    build [0x3D, 0x05, 0x0D, 0x15, 0x1D, 0x25, 0x2D] regArgs (one "dec") ++
    build [0x09, 0x19, 0x29, 0x39] reg16Args (one "add") ++
    build [0x03, 0x13, 0x23, 0x33] reg16Args (one "inc") ++
    build [0x0B, 0x1B, 0x2B, 0x3B] reg16Args (one "dec") ++
    build [0xF5, 0xC5, 0xD5, 0xE5] pushArgs (one "push") ++
    build [0xF1, 0xC1, 0xD1, 0xE1] pushArgs (one "pop") ++
    build [0xCD, 0xC4, 0xCC, 0xD4, 0xDC] flags (branch16 "call") ++
    build [0xC9, 0xC0, 0xC8, 0xD0, 0xD8] flags (ret) ++
    build [0xC7, 0xCF, 0xD7, 0xDF, 0xE7, 0xEF, 0xF7, 0xFF] resetVecs (one "rst") ++
    build [0x18, 0x20, 0x28, 0x30, 0x38] flags (branch8 "jr") ++
    build [0xC3, 0xC2, 0xCA, 0xD2, 0xDA] flags (branch16 "jp") ++
    [(0x00, zero "nop"),
     (0x02, two "ld" (Address RegBC) RegA), 
     (0x07, zero "rlca"), 
     (0x08, (\ x y -> TwoArg "ld" (Address (Data16 $ Left $ to16 x y)) RegSP, 3)), 
     (0x0A, two "ld" RegA (Address RegBC)), 
     (0x0F, zero "rrca"),
     (0x12, two "ld" (Address RegDE) RegA), 
     (0x17, zero "rla"), 
     (0x1A, two "ld" RegA (Address RegDE)), 
     (0x1F, zero "rra"), 
     (0x22, two "ld" (Address RegHLInc) RegA), 
     (0x27, zero "daa"), 
     (0x2A, two "ld" RegA (Address RegHLInc)), 
     (0x2F, zero "cpl"),
     (0x32, two "ld" (Address RegHLDec) RegA),
     (0x34, one "inc" (Address RegHL)), 
     (0x35, one "dec" (Address RegHL)),
     (0x37, zero "scf"), 
     (0x3A, two "ld" RegA (Address RegHLDec)), 
     (0x36, ldimm "ld" (Address RegHL)), 
     (0x3F, zero "ccf"),
     (0x76, zero "halt"),
     (0xD9, zero "reti"),
     (0xE0, ldh $ \arg -> TwoArg "ld" arg RegA), 
     (0xE2, two "ld" (Address PlusC) RegA), 
     (0xE8, (\ x _ -> TwoArg "add" RegSP (Signed8 $ signed x), 2)), 
     (0xE9, (\ _ _ -> BranchTwo "jp" Unconditional (Address RegHL), 1)),
     (0xEA, (\ x y -> TwoArg "ld" (Address $ Data16 $ Left $ to16 x y) RegA, 3)),
     (0xF0, ldh $ \arg -> TwoArg "ld" RegA arg), 
     (0xF2, two "ld" RegA (Address PlusC)), 
     (0xF3, zero "di"), 
     (0xF8, (\ x _ -> TwoArg "ld" RegHL (RegSPPlus $ signed x), 2)),
     (0xF9, two "ld" RegSP RegHL), 
     (0xFA, (\ x y -> TwoArg "ld" RegA (Address $ Data16 $ Left $ to16 x y), 3)),
     (0xFB, zero "ei")] ++
     zip [0x10, 0xCB, 0xD3, 0xDB, 0xDD, 0xE3, 0xE4, 0xEB, 0xEC, 0xED, 0xF4, 0xFC, 0xFD] 
         (repeat $ zero "undefined")


rotateOps = ["rlc", "rrc", "rl", "rr", "sla", "sra", "swap", "srl"]
bitOps = map Const [0..7]
-- bitOps = map bitOp [0..7]
-- resOps = map res [0..7]
-- setOps = map set [0..7]

cbInstructions :: [(Word8, InstrInfo)]
cbInstructions = 
    build [0x1F, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D] regArgs (one "rr") ++
    build [0x0F, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D] regArgs (one "rrc") ++
    build [0x3F, 0x38, 0x39, 0x3A, 0x3B, 0x3C, 0x3D] regArgs (one "srl") ++
    build [0x2F, 0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D] regArgs (one "sra") ++ 
    build [0x17, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15] regArgs (one "rl") ++
    build [0x07, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05] regArgs (one "rlc") ++
    build [0x27, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25] regArgs (one "sla") ++
    build [0x37, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35] regArgs (one "swap") ++
    build [0x47, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45] regArgs (two "bit" (Const 0)) ++
    build [0x4F, 0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D] regArgs (two "bit" (Const 1)) ++
    build [0x57, 0x50, 0x51, 0x52, 0x53, 0x54, 0x55] regArgs (two "bit" (Const 2)) ++
    build [0x5F, 0x58, 0x59, 0x5A, 0x5B, 0x5C, 0x5D] regArgs (two "bit" (Const 3)) ++
    build [0x67, 0x60, 0x61, 0x62, 0x63, 0x64, 0x65] regArgs (two "bit" (Const 4)) ++
    build [0x6F, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D] regArgs (two "bit" (Const 5)) ++
    build [0x77, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75] regArgs (two "bit" (Const 6)) ++
    build [0x7F, 0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D] regArgs (two "bit" (Const 7)) ++
    build [0x87, 0x80, 0x81, 0x82, 0x83, 0x84, 0x85] regArgs (two "res" (Const 0)) ++
    build [0x8F, 0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D] regArgs (two "res" (Const 1)) ++
    build [0x97, 0x90, 0x91, 0x92, 0x93, 0x94, 0x95] regArgs (two "res" (Const 2)) ++
    build [0x9F, 0x98, 0x99, 0x9A, 0x9B, 0x9C, 0x9D] regArgs (two "res" (Const 3)) ++
    build [0xA7, 0xA0, 0xA1, 0xA2, 0xA3, 0xA4, 0xA5] regArgs (two "res" (Const 4)) ++
    build [0xAF, 0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD] regArgs (two "res" (Const 5)) ++
    build [0xB7, 0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5] regArgs (two "res" (Const 6)) ++
    build [0xBF, 0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD] regArgs (two "res" (Const 7)) ++
    build [0xC7, 0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5] regArgs (two "set" (Const 0)) ++
    build [0xCF, 0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD] regArgs (two "set" (Const 1)) ++
    build [0xD7, 0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5] regArgs (two "set" (Const 2)) ++
    build [0xDF, 0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD] regArgs (two "set" (Const 3)) ++
    build [0xE7, 0xE0, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5] regArgs (two "set" (Const 4)) ++
    build [0xEF, 0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED] regArgs (two "set" (Const 5)) ++
    build [0xF7, 0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5] regArgs (two "set" (Const 6)) ++
    build [0xFF, 0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD] regArgs (two "set" (Const 7)) ++
    build [0x06, 0x0E, 0x16, 0x1E, 0x26, 0x2E, 0x36, 0x3E] rotateOps (flip one aHL) ++
    build [0x46, 0x4E, 0x56, 0x5E, 0x66, 0x6E, 0x76, 0x7E] bitOps (flip (two "bit") aHL) ++
    build [0x86, 0x8E, 0x96, 0x9E, 0xA6, 0xAE, 0xB6, 0xBE] bitOps (flip (two "res") aHL) ++
    build [0xC6, 0xCE, 0xD6, 0xDE, 0xE6, 0xEE, 0xF6, 0xFE] bitOps (flip (two "set") aHL)


-- The decoder

decoder :: Map.Map Word8 (Word8 -> Word8 -> Instruction, Int)
decoder = Map.fromList instructions

cbDecoder :: Map.Map Word8 (Word8 -> Word8 -> Instruction, Int)
cbDecoder = Map.fromList cbInstructions


decode :: Word8 -> Word8 -> Word8 -> (Instruction, Int)
decode 0xCB b2 b3 = 
    let Just (f, x) = Map.lookup b2 cbDecoder in (f b3 undefined, x+1)
decode b1 b2 b3 = 
    let Just (f, x) = Map.lookup b1 decoder in (f b2 b3, x)

decodeGB :: PureGB -> Word16 -> (Instruction, Int)
decodeGB gb addr = decode b1 b2 b3
    where [b1, b2, b3] = evalState (mapM readMem [addr, addr+1, addr+2]) gb

-- Annotate an instruction with symbols
    

argSymbs :: Argument -> Bimap.Bimap String Word16 -> Argument
argSymbs (Data16 (Left x)) symbols = 
        Data16 $ case Bimap.lookupR x symbols of Just y  -> Right y
                                                 Nothing -> Left x
argSymbs (Address x) symbols = Address $ argSymbs x symbols
argSymbs x _ = x

jrSymbs :: Word16 -> Argument -> Bimap.Bimap String Word16 -> Argument
jrSymbs addr (Data8 x) symbols = argSymbs translated symbols
    where translated = Data16 $ Left $ addr + (fromIntegral (2 + signed x))
jrSymbs _ arg _ = arg

applySymbols :: Word16 -> Bimap.Bimap String Word16 -> Instruction -> Instruction
applySymbols _    s (OneArg op x)   = OneArg op (argSymbs x s)
applySymbols _    s (TwoArg op x y) = TwoArg op (argSymbs x s) (argSymbs y s)
applySymbols addr s (BranchTwo "jr" x y) = BranchTwo "jr" x (jrSymbs addr y s)
applySymbols _    s (BranchTwo op x y) = BranchTwo op x (argSymbs y s)
applySymbols _    _ x = x


decodeGBSymb :: Bimap.Bimap String Word16 -> PureGB -> Word16 -> (Instruction, Int)
decodeGBSymb symbs gb addr = 
    let (i, x) = decodeGB gb addr in (applySymbols addr symbs i, x)
