module Sound where

import Data.Word
import Data.STRef
import Data.Bits
import Data.Ratio
import Debug.Trace
import qualified Data.DList as DList
import qualified Data.Vector.Unboxed as VU
import Control.Monad.RWS.Strict

import Types
import CpuBasic

---- Channel variable information

data ChannelVars s = ChannelVars
    {
        chPlaying      :: STGB s -> STRef s Bool,
        chFreqIdx      :: STGB s -> STRef s Frac,
        chFrequency    :: STGB s -> STRef s Int,
        chFreqConvert  :: Int -> Int,
        chVolume       :: STGB s -> STRef s Int,
        chVolPeriod    :: STGB s -> STRef s Int,
        chVolCounter   :: STGB s -> STRef s Int,
        chVolDirection :: STGB s -> STRef s Int,
        chDuty         :: STGB s -> STRef s Int,
        chWave         :: GBState s (VU.Vector Word8),
        chLength       :: STGB s -> STRef s Int,
        chFreqSwpTime  :: STGB s -> STRef s Int,
        chFreqSwpDir   :: STGB s -> STRef s Int,
        chFreqSwpShift :: STGB s -> STRef s Int,
        chFreqSwpCntr  :: STGB s -> STRef s Int,

        chR0 :: STGB s -> STRef s Word8,
        chR1 :: STGB s -> STRef s Word8,
        chR2 :: STGB s -> STRef s Word8,
        chR3 :: STGB s -> STRef s Word8,
        chR4 :: STGB s -> STRef s Word8
    }


-- With -XStrict we cannot put undefined into a ChannelVars record. So we use
-- notCapable instead
notCapable :: STGB s -> STRef s a
notCapable _ = undefined


channelVars :: Channel -> ChannelVars s
channelVars Channel1 = ChannelVars ch1Playing
                                   ch1FreqIdx 
                                   ch1Frequency 
                                   sqChConverter
                                   ch1Volume 
                                   ch1VolPeriod
                                   ch1VolCounter
                                   ch1VolDirection
                                   ch1Duty 
                                   (duty <$> askRef ch1Duty)
                                   ch1Length
                                   ch1FreqSwpTime  
                                   ch1FreqSwpDir   
                                   ch1FreqSwpShift 
                                   ch1FreqSwpCntr  
                                   regNR10
                                   regNR11
                                   regNR12
                                   regNR13
                                   regNR14
channelVars Channel2 = ChannelVars ch2Playing
                                   ch2FreqIdx 
                                   ch2Frequency 
                                   sqChConverter
                                   ch2Volume 
                                   ch2VolPeriod
                                   ch1VolCounter
                                   ch2VolDirection
                                   ch2Duty 
                                   (duty <$> askRef ch2Duty)
                                   ch2Length
                                   notCapable
                                   notCapable
                                   notCapable
                                   notCapable
                                   notCapable
                                   regNR21
                                   regNR22
                                   regNR23
                                   regNR24
channelVars Wave     = ChannelVars chWPlaying
                                   chWFreqIdx 
                                   chWFrequency 
                                   waveChConverter 
                                   chWVolume 
                                   notCapable -- ch1VolPeriod
                                   notCapable -- ch1VolCounter
                                   notCapable -- ch1VolDirection
                                   notCapable -- ch1Duty 
                                   waveDuty 
                                   chWLength
                                   notCapable -- ch1FreqSwpTime  
                                   notCapable -- ch1FreqSwpDir   
                                   notCapable -- ch1FreqSwpShift 
                                   notCapable -- ch1FreqSwpCntr  
                                   regNR30
                                   regNR31
                                   regNR32
                                   regNR33
                                   regNR34
channelVars Noise    = ChannelVars chNPlaying
                                   chNFreqIdx 
                                   chNFrequency 
                                   noiseChConverter
                                   chNVolume 
                                   chNVolPeriod
                                   chNVolCounter
                                   chNVolDirection
                                   notCapable 
                                   noiseDuty
                                   chNLength
                                   notCapable 
                                   notCapable 
                                   notCapable 
                                   notCapable 
                                   notCapable 
                                   regNR41
                                   regNR42
                                   regNR43
                                   regNR44


---- Reading and writing sound registers

readMask :: (STGB s -> STRef s Word8) -> Word8 -> GBState s Word8
readMask reg mask = do
    val <- askRef reg
    return $ (val .&. mask) .|. complement mask

readNR10 = readMask regNR10 0x7F
readNR11 = readMask regNR11 0xC0
readNR12 = readMask regNR12 0xFF
readNR13 = readMask regNR13 0x00
readNR14 = readMask regNR14 0x40

readNR21 = readMask regNR21 0xC0
readNR22 = readMask regNR22 0xFF
readNR23 = readMask regNR23 0x00
readNR24 = readMask regNR24 0x40

readNR30 = readMask regNR30 0x80
readNR31 = readMask regNR31 0x00
readNR32 = readMask regNR32 0x60
readNR33 = readMask regNR33 0x00
readNR34 = readMask regNR34 0x40

readNR41 = readMask regNR41 0x00
readNR42 = readMask regNR42 0xFF
readNR43 = readMask regNR43 0xFF 
readNR44 = readMask regNR44 0x40

readNR50 = readMask regNR50 0xFF
readNR51 = readMask regNR51 0xFF
readNR52 = readMask regNR52 0xFF


writeNR10 x = doSound >> putRef regNR10 x >> copyFreqSweep Channel1
writeNR11 x = doSound >> putRef regNR11 x >> copyLength Channel1 >> copyDuty Channel1
writeNR12 x = doSound >> putRef regNR12 x >> copyVolume Channel1
writeNR13 x = doSound >> putRef regNR13 x >> copyFreq Channel1
writeNR14 x = doSound >> putRef regNR14 x >> copyFreq Channel1 >> copyPlaying Channel1
                         
writeNR21 x = doSound >> putRef regNR21 x >> copyLength Channel2 >> copyDuty Channel2
writeNR22 x = doSound >> putRef regNR22 x >> copyVolume Channel2
writeNR23 x = doSound >> putRef regNR23 x >> copyFreq Channel2
writeNR24 x = doSound >> putRef regNR24 x >> copyFreq Channel2 >> copyPlaying Channel2

writeNR30 x = doSound >> putRef regNR30 x >> copyDACEnable
writeNR31 x = doSound >> putRef regNR31 x >> copyWaveLength
writeNR32 x = doSound >> putRef regNR32 x >> copyWaveVolume
writeNR33 x = doSound >> putRef regNR33 x >> copyFreq Wave
writeNR34 x = doSound >> putRef regNR34 x >> copyFreq Wave >> copyPlaying Wave

writeNR41 x = doSound >> putRef regNR41 x >> copyLength Noise
writeNR42 x = doSound >> putRef regNR42 x >> copyVolume Noise
writeNR43 x = doSound >> putRef regNR43 x >> copyFreqNoise >> copyWaveNoise
writeNR44 x = doSound >> putRef regNR44 x >> copyPlaying Noise


writeNR50 = putRef regNR50 
writeNR51 = putRef regNR51 
writeNR52 = putRef regNR52 

    

---- Dealing with input from the CPU

copyFreqSweep :: Channel -> GBState s ()
copyFreqSweep channel = do
    let vars = channelVars channel
    r0 <- askRef (chR0 vars)

    -- Time is bits 6-4, direction is bit 3, sweep shift is bits 2-0
    let time = (fromIntegral $ shift (r0 .&. 0x70) (-4)) :: Int
        dir  = if testBit r0 3 then -1 else 1
        num  = fromIntegral $ r0 .&. 0x07
    putRef (chFreqSwpTime  vars) time
    putRef (chFreqSwpDir   vars) dir
    putRef (chFreqSwpShift vars) num
    putRef (chFreqSwpCntr  vars) 0


copyLength :: Channel -> GBState s ()
copyLength channel = do
    let vars = channelVars channel
    r1 <- askRef (chR1 vars)

    -- Length is 64 minus the value in bits 5 through 0 of r1
    let len = fromIntegral $ 64 - (r1 .&. 0x3F)
    putRef (chLength vars) len

copyWaveLength :: GBState s ()
copyWaveLength = do
    r1 <- askRef (chR1 $ channelVars Wave)

    -- Length is 256 minus the value in bits 5 through 0 of r1
    let len = 256 - fromIntegral r1
    putRef (chLength $ channelVars Wave) len


copyDuty :: Channel -> GBState s ()
copyDuty channel = do
    let vars = channelVars channel
    r1 <- askRef (chR1 vars)

    -- Wave duty pattern is bits 7 and 6 of r1
    let pattern = fromIntegral $ shift (r1 .&. 0xC0) (-6)
    putRef (chDuty vars) pattern



copyVolume :: Channel -> GBState s ()
copyVolume channel = do
    let vars = channelVars channel
    r2 <- askRef (chR2 vars)

    -- Volume is bits 7-4 of r2, sweep in bit 3, sweep duration in bits 2-0
    let volume = fromIntegral $ shift (r2 .&. 0xF0) (-4)
        volDir = if testBit r2 3 then 1 else (-1)
        volPer = fromIntegral $ (r2 .&. 7)
    putRef (chVolume vars) volume
    putRef (chVolDirection vars) volDir
    putRef (chVolPeriod vars) volPer
    putRef (chVolCounter vars) 0

copyWaveVolume :: GBState s ()
copyWaveVolume = do
    r2 <- askRef (chR2 $ channelVars Wave)

    let volCode = fromIntegral $ shiftR (r2 .&. 0x60) 5
    putRef (chWVolCode) volCode



copyFreq :: Channel -> GBState s ()
copyFreq channel = do
    let vars = channelVars channel
    
    r3 <- askRef (chR3 vars)
    r4 <- askRef (chR4 vars)

    -- Frequency is first 3 bits of r4 and r3
    let freqHigh = fromIntegral (r4 .&. 7) :: Int
        freq     = freqHigh * 256 + fromIntegral r3
    putRef (chFrequency vars) freq

copyFreqNoise :: GBState s ()
copyFreqNoise = do
    -- Freq = 524288 Hz / r / 2^(s+1); For r=0 assume r=0.5 instead 
    -- s is bits 7-4, r is bits 2-0 
    r3 <- askRef $ chR3 (channelVars Noise)
    let s = fromIntegral $ shift (r3 .&. 0xF0) (-4)
        r = fromIntegral r3 .&. 0x07
        div1 = if r == 0 then (524288 * 2) else (524288 `div` r)
        div2 = shift div1 $ -(s + 1)
    putRef (chFrequency $ channelVars Noise) div2

copyWaveNoise :: GBState s ()
copyWaveNoise = do
    -- Bit 3 of r3: low means lfsr length of 15, high means lfsr length of 6
    r3 <- askRef $ chR3 (channelVars Noise)
    let l = testBit r3 3
    putRef chNShortDuty l

copyDACEnable :: GBState s ()
copyDACEnable = do
    r0 <- askRef $ chR0 (channelVars Wave)
    putRef chWDACEnable $ testBit r0 7


copyPlaying :: Channel -> GBState s ()
copyPlaying channel = do
    let vars = channelVars channel
    r4 <- askRef (chR4 vars)

    let playing = testBit r4 7
    when playing $ do
        putRef (chPlaying vars) True
        putRef (chFreqIdx vars) 0

    -- If length counter is zero, it is reloaded
    len <- askRef (chLength vars)
    when (len == 0) $ putRef (chLength vars) (if channel == Wave then 256 else 64)



---- The noise channel

lfsr :: Int -> Int -> Int
lfsr val width = 
    let 
        shifted = shift val (-1)
        newBit  = testBit (val `xor` shifted) 0
    in
        if newBit then setBit shifted (width - 1) else shifted

generateNoise :: Int -> VU.Vector Word8
generateNoise width = 
    let
        generator x = x : (generator $ lfsr x width)
        toSample  x = fromIntegral $ 1 - x .&. 1 -- the wave is bit 0 of the lfsr inverted
        initial = 2^width - 1
        wave    = toSample <$> generator initial 
    in
        VU.fromList (take initial wave)

noise15 :: VU.Vector Word8
noise15 = generateNoise 15

noise6 :: VU.Vector Word8
noise6  = generateNoise 6

noiseDuty :: GBState s (VU.Vector Word8)
noiseDuty = do
    short <- askRef chNShortDuty 
    return $ if short then noise6 else noise15


---- The wave channel

applyWaveVolume :: Samples -> GBState s Samples
applyWaveVolume samples = do
    code <- askRef chWVolCode

    let modifier = if | code == 0 -> const 0
                      | code == 1 -> id
                      | code == 2 -> (`div` 2)
                      | code == 3 -> (`div` 4)
                      | otherwise -> error "BUG: wave volume code not in [0,3]"

    return $ VU.map modifier samples


waveDuty :: GBState s (VU.Vector Word8)
waveDuty = do
    vec <- asks wavePattern
    VU.freeze vec

---- Generating square waves

silence :: VU.Vector Word8
silence = VU.fromList [0,0,0,0,0,0,0,0]

duty :: Int -> VU.Vector Word8
duty 0 = VU.fromList [0,0,0,0,0,0,0,1]
duty 1 = VU.fromList [1,0,0,0,0,0,0,1]
duty 2 = VU.fromList [1,0,0,0,0,1,1,1]
duty 3 = VU.fromList [0,1,1,1,1,1,1,0]
duty _ = error "impossible duty cycle"

gbClock :: Int
gbClock = 4194304

cyclesPerSample :: Frac
cyclesPerSample = fromIntegral gbClock / 48000

sqChConverter :: Int -> Int
sqChConverter freq = (2048 - fromIntegral freq) * 4 

noiseChConverter :: Int -> Int
noiseChConverter freq = gbClock `div` freq

waveChConverter :: Int -> Int
waveChConverter freq =  (2048 - fromIntegral freq) * 2


freqsPerSample :: Int -> Frac
freqsPerSample freq = cyclesPerSample / fromIntegral freq

fracMod :: Int -> Frac -> Frac
fracMod m x = (\(x, y) -> fromIntegral ((x :: Int) `mod` m) + y) $ properFraction x


sample :: VU.Vector Word8 -> Int -> Int -> Int -> Frac -> (Samples, Frac)
sample wave volume samples freq initIdx = 
    let
        l   = VU.length wave
        inc = freqsPerSample freq 
        sampleAtIdx x = 
            let 
                fracIdx = inc * fromIntegral x + initIdx
                intIdx  = (fst . properFraction $ fracIdx) `mod` l
            in  (wave VU.! intIdx) * fromIntegral volume
        sampled = VU.generate samples sampleAtIdx
        
        next = fracMod l $ initIdx + inc * fromIntegral samples
    in
        (sampled, next)


outputL :: Samples -> GBState s ()
outputL wave = modify $ \ out -> out { soundL = soundL out `DList.snoc` wave }

outputR :: Samples -> GBState s ()
outputR wave = modify $ \ out -> out { soundR = soundR out `DList.snoc` wave }


generate :: Channel -> Int -> GBState s Samples
generate channel samples = do
    let vars  = channelVars channel
        freqf = chFreqConvert vars

    -- Generate the wave
    playing <- if channel /= Wave 
                   then askRef $ chPlaying vars 
                   else liftM2 (&&) (askRef chWPlaying) (askRef chWDACEnable)
    wave <- if playing then chWave vars
                       else return silence
    vol  <- askRef $ chVolume vars
    idx  <- askRef $ chFreqIdx vars
    freq <- freqf <$> (askRef $ chFrequency vars)

    let (waveData, newIdx) = sample wave vol samples freq idx
    putRef (chFreqIdx vars) newIdx

    if channel /= Wave 
        then return waveData
        else applyWaveVolume waveData


mix :: Int -> [Samples] -> Samples
mix n samples = VU.generate n mixf
    where mixf i = sum $ map (VU.! i) samples


doSound :: GBState s ()
doSound = do
    -- How much sound should we generate?
    last <- askRef lastSound 
    now  <- askRef cycles
    let cyc = fromIntegral $ now - last
    putRef lastSound now

    -- How many samples should we take to hit 48000 samples/sec?
    sRem <- askRef sampleRemainder
    let nSamplesFrac     = sRem + cyc * 48000 / fromIntegral gbClock
        (samples, sRem') = properFraction nSamplesFrac
    putRef sampleRemainder sRem'

    -- Generate waves
    ch1Wave <- generate Channel1 samples
    ch2Wave <- generate Channel2 samples
    chWWave <- generate Wave samples
    chNWave <- generate Noise samples

    -- Route channels left and right
    lr <- askRef regNR51
    let stereoFilter = map snd . filter (\(x,_) -> testBit lr x)
        left  = stereoFilter [(0, ch1Wave), (1, ch2Wave), (2, chWWave), (3, chNWave)]
        right = stereoFilter [(4, ch1Wave), (5, ch2Wave), (6, chWWave), (7, chNWave)]

    outputL $ mix samples left
    outputR $ mix samples right

---- Frame sequencer

lengthCtrl :: Channel -> GBState s ()
lengthCtrl channel = do
    let vars = channelVars channel
    len <- askRef $ chLength vars


    -- Decrement length counter if NR4 asks us to
    r4 <- askRef (chR4 vars)
    when (testBit r4 6 && len /= 0) $ do
        let new = len - 1
        putRef (chLength vars) new

        -- If the length counter is now zero, stop the channel
        when (new == 0) $ do
            putRef (chPlaying vars) False



volumeCtrl :: Channel -> GBState s ()
volumeCtrl channel = do
    let vars = channelVars channel
       
    period  <- askRef $ chVolPeriod vars
    when (period /= 0) $ do
        counter <- askRef $ chVolCounter vars
        putRef (chVolCounter vars) $ (counter + 1) `mod` period

        when (counter + 1 == period) $ do
            volume    <- askRef $ chVolume vars
            direction <- askRef $ chVolDirection vars

            let newVol  = volume + direction

            if newVol <= 15 && newVol >= 0 then
                putRef (chVolume vars) newVol
            else 
                -- volume sweep is disabled once volume hits the limit
                putRef (chVolPeriod vars) $ 0

            
frequencyCtrl :: Channel -> GBState s ()
frequencyCtrl channel = do
    let vars = channelVars channel

    period <- askRef $ chFreqSwpTime vars
    when (period /= 0) $ do
        counter <- askRef $ chFreqSwpCntr vars
        putRef (chFreqSwpTime vars) $ (counter + 1) `mod` period

        when (counter + 1 == period) $ do
            freq <- askRef $ chFrequency    vars
            dir  <- askRef $ chFreqSwpDir   vars
            shft <- askRef $ chFreqSwpShift vars
            let new = freq + dir * shift freq (-shft)

            if new <= 2047 then 
                putRef (chFrequency vars) new
            else
                -- channel is disabled if the overflow check is failed
                putRef (chPlaying vars) False




{-  
    Frame sequencer states
    ======================
    
    Step   Length Ctr  Vol Env     Sweep
    -------------------------------------
    0      Clock       -           -
    1      -           -           -
    2      Clock       -           Clock
    3      -           -           -
    4      Clock       -           -
    5      -           -           -
    6      Clock       -           Clock
    7      -           Clock       -
 -}

frameSequencer :: GBState s ()
frameSequencer = do
    doSound

    state <- askRef fsState
    putRef fsState $ (state + 1) `mod` 8

    when (state `mod` 2 == 0) $
        mapM_ lengthCtrl [Channel1, Channel2, Wave, Noise]

    when (state == 7) $
        mapM_ volumeCtrl [Channel1, Channel2, Noise]

    when (state == 2 || state == 6) $
        frequencyCtrl Channel1
