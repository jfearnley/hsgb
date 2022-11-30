module Cpu where

import Control.Monad.State.Strict
import Data.Word
import Data.Bits
import Debug.Trace

import Types
import Memory
import Util

import CpuBasic


---- Read from/write to memory pointed to by a register

readMemReg :: Read16 s -> GBState s Word8
readMemReg reg = reg >>= readMem

[readMemBC, readMemDE, readMemHL, readMemPC, readMemSP] = 
    map readMemReg [readBC, readDE, readHL, readPC, readSP]


writeMemReg :: Read16 s -> Word8 -> GBState s ()
writeMemReg reg val = do{addr <- reg; writeMem addr val}

[writeMemBC, writeMemDE, writeMemHL, writeMemPC, writeMemSP] = 
    map writeMemReg [readBC, readDE, readHL, readPC, readSP]


---- Operations used in instruction decoding and execution

pcPlus :: Word8 -> Read8 s
pcPlus n = readPC >>= \pc -> readMem (pc + fromIntegral n)

pcPlus16 :: Word8 -> Read16 s
pcPlus16 n = do{b1 <- pcPlus n; b2 <- pcPlus (n+1); return $ to16 b1 b2}



---- Load Instructions

-- ld *,n
loadImmediate :: Write8 s -> GBState s ()
loadImmediate write = pcPlus 1 >>= write >> incCyc 8 >> incPC 2

-- ld *,*
loadR2R :: Write8 s -> Read8 s -> GBState s ()
loadR2R write read = read >>= write >> incCyc 4 >> incPC 1

-- ld **,nn
load16Immediate :: Write16 s -> GBState s ()
load16Immediate write = pcPlus16 1 >>= write >> incCyc 12 >> incPC 3

---- Memory operations

-- ld (hl),*
hlWrite :: Read8 s -> GBState s ()
hlWrite read = read >>= writeMemHL >> incCyc 8 >> incPC 1

-- ld *,(hl)
hlRead :: Write8 s -> GBState s ()
hlRead write = readMemHL >>= write >> incCyc 8 >> incPC 1


---- Addition

plusFlags :: Word8 -> Word8 -> GBState s Word8
plusFlags a b = setFlags False zero carry hcarry >> return result
    where result = a + b 
          zero = result == 0
          carry = (fromIntegral a + fromIntegral b) > (255 :: Int)
          hcarry = (a .&. 0x0F) + (b .&. 0x0F) > 0x0F


-- add *
add :: Read8 s -> GBState s ()
add reg = reg >>= (\val -> readA >>= plusFlags val >>= writeA) 
                                                >> incCyc 8 >> incPC 1


-- only called if carry is True
addCarry :: GBState s ()
addCarry = do
    a <- readA
    c <- getCarry
    hc <- getHCarry
    writeA (a+1)
    let zero = a+1 == 0
        carry = (fromIntegral a + 1 > (0xFF :: Int)) || c
        hcarry = (a .&. 0x0F) + 1 > 0x0F || hc
    setFlags False zero carry hcarry


-- adc *
adc :: Read8 s -> GBState s ()
adc reg = do {c <- getCarry; add reg; when c addCarry}

---- Subtraction

minusFlags :: Word8 -> Word8 -> GBState s Word8
minusFlags a b = setFlags True zero carry hcarry >> return result
    where result = a - b
          zero = result == 0
          carry = (fromIntegral a - fromIntegral b) < (0 :: Int)
          hcarry = (a .&. 0x0F) - (b .&. 0x0F) > 0x0F

-- sub *
sub :: Read8 s -> GBState s ()
sub reg = reg >>= (\val -> readA >>= (`minusFlags` val) >>= writeA) 
                                                >> incCyc 4 >> incPC 1



-- only called if carry is True
subCarry :: GBState s ()
subCarry = do
    a <- readA
    c <- getCarry
    hc <- getHCarry
    writeA (a-1)
    let inta = fromIntegral a :: Int
        zero = a-1 == 0
        carry = inta - 1 < 0 || c
        hcarry = (inta .&. 0x0F) - 1 < 0 || hc
    setFlags True zero carry hcarry

-- sbc *
sbc :: Read8 s -> GBState s ()
sbc reg = do {c <- getCarry; sub reg; when c subCarry}



----- Logic

logicFlags :: Word8 -> GBState s ()
logicFlags result = 
    setZero (result == 0) >> setNegative False >> setCarry False >> setHCarry False

logicOp :: (Word8 -> Word8 -> Word8) -> Word8 -> GBState s Word8
logicOp op val = do a <- readA
                    let result = a `op` val
                    logicFlags result
                    return result

-- and *
andOp :: Read8 s -> GBState s ()
andOp reg = reg >>= logicOp (.&.) >>= writeA >> setHCarry True >>
                                                            incCyc 4 >> incPC 1
-- or *
orOp :: Read8 s -> GBState s ()
orOp reg = reg >>= logicOp (.|.) >>= writeA >> incCyc 4 >> incPC 1

-- xor *
xorOp :: Read8 s -> GBState s ()
xorOp reg = reg >>= logicOp xor >>= writeA >> incCyc 4 >> incPC 1


---- Compare

-- cp *
cp :: Read8 s -> GBState s ()
cp reg = reg >>= (\val -> readA >>= (`minusFlags` val)) >> incCyc 4 >> incPC 1


---- Increments and decrements

data IncDec = Inc | Dec deriving Eq

incDec :: IncDec -> Word8 -> GBState s Word8
incDec increment regval = do 
    let result = case increment of Inc -> regval + 1
                                   Dec -> regval - 1
        hcarry Inc = regval .&. 0x0F == 0x0F
        hcarry Dec = regval .&. 0x0F == 0
    setZero (result == 0)
    setNegative (increment /= Inc)
    setHCarry (hcarry increment)
    return result

-- inc *
inc :: (Read8 s, Write8 s) -> GBState s ()
inc (read, write) = read >>= incDec Inc >>= write >> incCyc 4 >> incPC 1

-- dec *
dec :: (Read8 s, Write8 s) -> GBState s ()
dec (read, write) = read >>= incDec Dec >>= write >> incCyc 4 >> incPC 1


---- Right shifts

-- If rotate = True, then rotate through carry
-- Otherwise, shift into carry
rrcOp :: Bool -> (Read8 s, Write8 s) -> GBState s ()
rrcOp rotate (reg, out) = do 
    x <- reg
    c <- getCarry
    let cout = if c && rotate then bit 7 else 0 
        res  = shiftR x 1 .|. cout 
        new_carry = testBit x 0
        zero = res == 0
    setFlags False zero new_carry False
    out res
    incCyc 8 >> incPC 2

-- Rotates to the right and stores old bit zero in carry
rrOp :: (Read8 s, Write8 s) -> GBState s ()
rrOp (reg, out) = do
    x <- reg
    setFlags False (x == 0) (testBit x 0) False
    out $ rotate x (-1)
    incCyc 8 >> incPC 2

-- rr *
rr :: (Read8 s, Write8 s) -> GBState s ()
rr = rrcOp True 

-- rrc *
rrc = rrOp

-- srl *
srl = rrcOp False

-- sra *
sra (read, write) = do 
    x <- read
    srl (read, write)
    x' <- read
    let newx = x' .|. (x .&. bit 7)
    write newx >> setZero (newx == 0)

-- rra  
rra = rrcOp True (readA, writeA) >> setZero False >> decCyc 4 >> decPC 1

-- rrca
rrca = rrOp (readA, writeA) >> setZero False >> decCyc 4 >> decPC 1

---- Left shifts

-- If rotate is true then shift left through carry
-- Otherwise shift left into carry
rlcOp :: Bool -> (Read8 s, Write8 s) -> GBState s ()
rlcOp rotate (reg, out) = do 
    x <- reg
    c <- getCarry
    let cout = if c && rotate then 1 else 0 
        res  = shiftL x 1 .|. cout 
        new_carry = testBit x 7
        zero = res == 0
    setFlags False zero new_carry False
    out res
    incCyc 8 >> incPC 2

-- Rotate left ignoring carry
rlOp :: (Read8 s, Write8 s) -> GBState s ()
rlOp (reg, out) = do
    x <- reg
    setFlags False (x == 0) (testBit x 7) False
    out $ rotate x 1 
    incCyc 8 >> incPC 2

-- rl *
rl = rlcOp True

-- sla *
sla = rlcOp False

-- rlc *
rlc :: (Read8 s, Write8 s) -> GBState s ()
rlc = rlOp

-- rla 
rla = rlcOp True (readA, writeA) >> setZero False >> decCyc 4 >> decPC 1

-- rlca
rlca = rlOp (readA, writeA) >> setZero False >> decCyc 4 >> decPC 1


bitHL :: ((Read8 s, Write8 s) -> GBState s ()) -> GBState s ()
bitHL op = op (readMemHL, writeMemHL) >> incCyc 8

---- Bit operations

-- bit n,*
bitOp :: Int -> Read8 s -> GBState s ()
bitOp i reg = reg >>= (\ x -> setZero (not $ testBit x i)) >> 
                    setNegative False >> setHCarry True >> incCyc 8 >> incPC 2


-- set n,*
setOp :: Int -> Word8 -> GBState s Word8
setOp i x = return $ setBit x i

set :: Int -> (Read8 s, Write8 s) -> GBState s ()
set i (read, write) = read >>= setOp i >>= write >> incCyc 8 >> incPC 2


-- res n,*
resOp :: Int -> Word8 -> GBState s Word8
resOp i x = return $ clearBit x i

res :: Int -> (Read8 s, Write8 s) -> GBState s ()
res i (read, write) = read >>= resOp i >>= write >> incCyc 8 >> incPC 2

---- Swap

swap :: Word8 -> Word8
swap x = shiftL lower 4 .|. shiftR upper 4
    where lower = x .&. 0x0F
          upper = x .&. 0xF0


swapOp :: (Read8 s, Write8 s) -> GBState s ()
swapOp (reg, store) = do
    input <- reg
    let res = swap input
    store res
    setFlags False (res == 0) False False 
    incCyc 8 >> incPC 2



---- Arithmetic ops with memory and immediate operands

-- op (hl)
arithHL :: (Read8 s -> GBState s ()) -> GBState s ()
arithHL op = op readMemHL >> incCyc 4

-- op n
arithImm :: (Read8 s -> GBState s ()) -> GBState s ()
arithImm op = op (pcPlus 1) >> incCyc 4 >> incPC 1


---- 16 bit arithmetic

plusFlags16 :: Word16 -> Word16 -> GBState s Word16
plusFlags16 a b = do{z <- getZero; setFlags False z carry hcarry >> return result}
    where result = a + b
          carry = (fromIntegral a + fromIntegral b) > (65535 :: Int)
          hcarry = (a .&. 0x0FFF) + (b .&. 0x0FFF) > 0x0FFF

-- add **
add16 :: Read16 s -> GBState s ()
add16 reg = reg >>= (\val -> readHL >>= plusFlags16 val >>= writeHL) >> 
                                                            incCyc 8 >> incPC 1


incDec16 :: IncDec -> (Read16 s, Write16 s) -> GBState s ()
incDec16 inc (read, write) = read >>= op >> incCyc 8 >> incPC 1
    where op val = case inc of Inc -> write (val + 1)
                               Dec -> write (val - 1)

-- inc **
inc16 :: (Read16 s, Write16 s) -> GBState s ()
inc16 = incDec16 Inc

-- inc **
dec16 :: (Read16 s, Write16 s) -> GBState s ()
dec16 = incDec16 Dec


---- Decimal Adjust

-- Arguments are negative, carry, hcarry, register a
-- Returns the value to be added to a, and the new carry flag
daaOp :: Bool -> Bool -> Bool -> Word8 -> (Word8, Bool)
daaOp False c h a 
    | t1 && t2     = (0x66, True)
    | t1 && not t2 = (0x60, True)
    | not t1 && t2 = (0x06, c)
    | otherwise    = (0x00, c)
    where t1 = c || a > 0x99
          t2 = h || a .&. 0x0F > 0x09

daaOp True c h _
    | c && h     = (0x9A, c)
    | c && not h = (0xA0, c)
    | not c && h = (0xFA, c)
    | otherwise  = (0x00, c)


-- daa
daa :: GBState s ()
daa = do
    n <- getNegative
    c <- getCarry
    h <- getHCarry
    a <- readA
    let (ainc, nc) = daaOp n c h a
        newa = a + ainc
    writeA newa
    setCarry nc >> setHCarry False >> setZero (newa == 0)
    incCyc 4 >> incPC 1
    

---- Push

push :: Word16 -> GBState s ()
push val = do
    sp <- readSP
    let (low, high) = from16 val
    writeMem (sp - 1) high
    writeMem (sp - 2) low
    writeSP (sp - 2)

-- push **
pushOp :: Read16 s -> GBState s ()
pushOp reg = reg >>= push >> incCyc 16 >> incPC 1 


---- Pop

pop :: GBState s Word16
pop = do
    sp   <- readSP
    low  <- readMem sp
    high <- readMem (sp +1)
    writeSP (sp + 2)
    return $ to16 low high

-- pop **
popOp :: Write16 s -> GBState s ()
popOp reg = pop >>= reg >> incCyc 12 >> incPC 1



---- Calls and returns

call :: Word16 -> Bool -> GBState s ()
call addr True  = do {pc <- readPC; push (pc + 3)} >> writePC addr >> incCyc 24
call _    False = incCyc 12 >> incPC 3

-- call cond,nn
callOp :: GBState s Bool -> GBState s ()
callOp flag = do {b1 <- pcPlus 1; b2 <- pcPlus 2; f <- flag; 
                                                    call (to16 b1 b2) f}


ret :: Bool -> GBState s ()
ret True  = pop >>= writePC >> incCyc 20
ret False = incPC 1 >> incCyc 8

-- ret cond,nn
retOp :: GBState s Bool -> GBState s ()
retOp flag = flag >>= ret


-- rst nn
reset :: Word16 -> GBState s ()
reset addr = do {pc <- readPC; push (pc + 1)} >> writePC addr >> incCyc 16


---- Jumps 

jr :: Bool -> GBState s ()
jr False = incCyc 8 >> incPC 2
jr True  = do
    addr <- pcPlus 1
    let signedAddr = fromIntegral $ signed addr
    pc <- readPC
    writePC (pc + signedAddr + 2)
    incCyc 12

-- jr n
jrOp :: GBState s Bool -> GBState s ()
jrOp flag = flag >>= jr


jp :: Word16 -> Bool -> GBState s ()
jp addr True  = writePC addr >> incCyc 16
jp _    False = incPC 3      >> incCyc 12

-- jp nn
jpOp :: GBState s Bool -> GBState s ()
jpOp flag = do 
    addr <- pcPlus16 1 
    f <- flag
    jp addr f


---- Interrupts


-- di
di = writeIME False >> incPC 1 >> incCyc 4

-- ei
ei = do
    incPC 1 >> incCyc 4 
    ime <- readIME
    cyc <- readCycles
    unless ime $ do 
        putRef enableEI $ Just (cyc + 1)
        putRef eventsStale True

-- reti 
reti = ret True >> writeIME True >> putRef eventsStale True >> incCyc (-4)



       

---- Misc instructions

noop = incCyc 4 >> incPC 1 

halt = do
    next <- askRef nextEvent 
    case (next) of Nothing -> incCyc 4 -- we're halted forever more
                   Just x  -> putRef cycles x


--ld (hl),n
ldHL = pcPlus 1 >>= writeMemReg readHL >> incCyc 12 >> incPC 2


writeHLID op = readA >>= writeMemReg readHL >> op (readHL, writeHL) 

-- ld (hl+),a
writeHLI :: GBState s ()
writeHLI = writeHLID inc16

-- ld (hl-),a
writeHLD :: GBState s ()
writeHLD = writeHLID dec16


readHLID op = readHL >>= readMem >>= writeA >> op (readHL, writeHL) 
 
-- ld a,(hl+)
readHLI :: GBState s ()
readHLI = readHLID inc16

-- ld a,(hl-)
readHLD :: GBState s ()
readHLD = readHLID dec16

-- ld (bc),a
wrBC = readA >>= writeMemReg readBC >> incCyc 8 >> incPC 1

-- ld (de),a
wrDE = readA >>= writeMemReg readDE >> incCyc 8 >> incPC 1

-- ld (nn),a
wrNN = readA >>= writeMemReg (pcPlus16 1) >> incCyc 16 >> incPC 3

-- ld a,(bc)
reBC = readMemReg readBC >>= writeA >> incCyc 8 >> incPC 1

-- ld a,(de)
reDE = readMemReg readDE >>= writeA >> incCyc 8 >> incPC 1

-- ld a,(nn)
reNN = readMemReg (pcPlus16 1) >>= writeA >> incCyc 16 >> incPC 3

-- ld ($FF00+n),a
wrIO = do {n <- pcPlus 1; readA >>= writeMem (0xFF00 + fromIntegral n)} >> 
                                                            incCyc 12 >> incPC 2

-- ld a,($FF00+n)
reIO = do {n <- pcPlus 1; readMem (0xFF00 + fromIntegral n) >>= writeA} >>
                                                            incCyc 12 >> incPC 2

-- ld ($FF00+c),a
ldca = do {c <- readC; readA >>= writeMem (0xFF00 + fromIntegral c)} >> 
                                                            incCyc 8 >> incPC 1

-- ld a,($FF00+c)
ldac = do {c <- readC; readMem (0xFF00 + fromIntegral c) >>= writeA} >>
                                                            incCyc 8 >> incPC 1


-- inc (hl)
incHL = inc (readMemReg readHL, writeMemReg readHL) >> incCyc 8

-- dec (hl)
decHL = dec (readMemReg readHL, writeMemReg readHL) >> incCyc 8

-- jp (hl)
jpHL = readHL >>= writePC >> incCyc 4

-- ld (nn),sp
ldNNSP = do{addr <- pcPlus16 1; readSP >>= writeMem16 addr} >> incCyc 20 >> incPC 3

-- ld sp,hl
ldSPHL = readHL >>= writeSP >> incCyc 8 >> incPC 1

spALU :: GBState s Word16
spALU = do
    n <- pcPlus 1
    b <- readSP
    let
        a = fromIntegral (signed n)
        res = a + b
        carry = (a .&. 0x00FF) + (b .&. 0x00FF) > 0x00FF
        hcarry = (a .&. 0x000F) + (b .&. 0x000F) > 0x000F
    setZero False >> setNegative False >> setCarry carry >> setHCarry hcarry
    return res

-- add sp,n
addSP = spALU >>= writeSP >> incCyc 16 >> incPC 2

-- ldhl sp,n
ldhlSP = spALU >>= writeHL >> incCyc 12 >> incPC 2

-- cpl
cpl = readA >>= return . complement >>= writeA >> 
        setNegative True >> setHCarry True >> incCyc 4 >> incPC 1

-- ccf
ccf = getCarry >>= return . not >>= setCarry >>
        setNegative False >> setHCarry False >> incCyc 4 >> incPC 1

-- scf
scf = setCarry True >> setNegative False >> setHCarry False >> incCyc 4 >> incPC 1

-- unimplmented
unimpl = trace "executed unimplemented or undefined instruction" noop
