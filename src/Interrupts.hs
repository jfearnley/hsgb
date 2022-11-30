module Interrupts (interrupts, updateEvents) where

import Control.Monad
import Control.Monad.Loops
import Data.Bits
import Data.Maybe
import Data.List
import Data.Function

import Types
import CpuBasic
import Cpu
import Gpu
import Timer
import Memory
import Sound


interrupts :: GBState s ()
interrupts = do
    next  <- askRef nextEvent
    cyc   <- askRef cycles
    stale <- askRef eventsStale
    let trigger = case next of Nothing -> False
                               Just x  -> x <= cyc
    when (stale || trigger) updateEvents
    when stale $ putRef eventsStale False


---- Events

-- A timed event gives an action that should take place on a given clock
-- cycle 
type TimedEvent s = (Integer, GBState s ())


---- ei instruction delay

eiDelay :: GBState s (Maybe (TimedEvent s))
eiDelay = do
    ei <- askRef enableEI
    let action = do 
            putRef enableEI Nothing
            writeIME True
    return $ fmap (\x -> (x, action)) ei


---- Dispatching interrupts

dispatch :: GBState s (Maybe (TimedEvent s))
dispatch = do
    ime <- askRef ime
    ifv <- readIF
    iev <- readIE
    instr <- readMem =<< readPC
    let flags  = ifv .&. iev .&. 0x1F
        unhalt = when (instr == 0x76 && flags /= 0x00) $ incPC 1
        action = do
            -- If we are currently halted, then stop halting
            unhalt

            -- Which interrupt needs triggering?
            let id   = head [i | i <- [0..4], testBit flags i]
                addr = fromIntegral $ 0x0040 + id * 8

            -- Dispatch the interrupt
            writeIME False
            readIF >>= (\x -> return $ clearBit x id) >>= writeIF
            readPC >>= push
            writePC addr
            incCyc 20

    return $ if | ime     && flags /= 0x00 -> Just (0, action)
                | not ime && flags /= 0x00 && instr == 0x76 -> Just (0, unhalt)
                | otherwise -> Nothing

---- VBlank

vblankStart :: GBState s (Maybe (TimedEvent s))
vblankStart = do
    vb <- askRef vblankPeriod
    fs <- askRef frameStart
    let time   = fs + 65664
        action = do
                    -- Trigger vblank
                    setIFBit 0
                    putRef vblankPeriod True

                    -- Trigger stat mode 1 if applicable
                    enabled <- readSTATBit 4
                    lcdOn   <- regBit readLCDC 7
                    when (enabled && lcdOn) $ setIFBit 1
    return $ if vb then Nothing else Just (time, action) 


vblankEnd :: GBState s (Maybe (TimedEvent s))
vblankEnd = do
    vb <- askRef vblankPeriod
    fs <- askRef frameStart
    let time   = fs + 70224
        action = do
                    -- Update frame variables
                    modifyRef frameStart (+70224) 
                    modifyRef frameCount (+1) 
                    putRef vblankPeriod False

                    -- Reset to line 0
                    fs <- askRef frameStart
                    startOfLineAction
                    putRef lineStart fs
                    putRef lineCount 0
                    putRef windowLine 0

                    -- Check for ly=lyc=0 interrupt
                    enabled <- readSTATBit 6
                    lyc <- readLYC
                    lcdOn   <- regBit readLCDC 7
                    when (lcdOn && enabled && lyc == 0) $ setIFBit 1
    return $ if vb then Just (time, action) else Nothing


---- New scanline events

startOfLineAction = do
    -- New scanline, update state variables
    modifyRef lineStart (+456)
    modifyRef lineCount (+1)
    putRef renderTriggered False
    putRef stat0Triggered False
    putRef stat2Triggered False

    -- Check for ly=lyc interrupt
    lcdOn   <- regBit readLCDC 7
    enabled <- readSTATBit 6
    lyc     <- readLYC
    ly      <- readLY
    when (lcdOn && enabled && ly == lyc) $ setIFBit 1


startOfLine :: GBState s (Maybe (TimedEvent s))
startOfLine = do
    last <- askRef lineStart
    line <- askRef lineCount
    let time = last + 456
        action = startOfLineAction
    return $ if line > 144 then Nothing
                           else Just (time, action)
            

---- Stat interrupts

statZero :: GBState s (Maybe (TimedEvent s))
statZero = do
    mode0 <- readSTATBit 3
    line  <- askRef lineStart
    trig  <- askRef stat0Triggered
    let time   = line + 248
        action = putRef stat0Triggered True >> setIFBit 1
    return $ if (mode0 && not trig) then Just (time, action)
                                    else Nothing


statTwo :: GBState s (Maybe (TimedEvent s))
statTwo = do
    mode2 <- readSTATBit 5
    line  <- askRef lineStart
    trig  <- askRef stat2Triggered
    let time   = line + 80
        action = putRef stat2Triggered True >> setIFBit 1
    return $ if (mode2 && not trig) then Just (time, action)
                                    else Nothing

---- Rendering
            
render :: GBState s (Maybe (TimedEvent s))    
render = do
    last <- askRef lineStart
    line <- askRef lineCount
    trig <- askRef renderTriggered
    enabled <- askRef outputGraphics
    let time   = last + 248
        action = do
            putRef renderTriggered True
            when (line < 144) $ processLine line
    return $ if not trig && enabled then Just (time, action)
                                    else Nothing


---- Timer


timer :: GBState s (Maybe (TimedEvent s))
timer = do
    enabled <- timerEnabled 
    if (not enabled) then return Nothing
    else do
        init    <- askRef regTIMA
        started <- askRef timerStarted
        rate    <- timerClock
        let time = started + rate * (256 - fromIntegral init)
            action = do
                setIFBit 2
                putRef timerStarted time
                readTMA >>= (putRef regTIMA)
        return $ Just (time, action)

---- Sound

soundFS :: GBState s (Maybe (TimedEvent s))
soundFS = do
    last <- askRef lastFS
    let time   = last + 8192 -- 512 Hz
        action = do
            putRef lastFS time
            frameSequencer
    return $ Just (time, action)


-- The master list of events that might trigger
timedEvents = [eiDelay, dispatch, timer, startOfLine, statZero, statTwo, render, vblankStart, vblankEnd, soundFS]

-- Figure out which events need running and run them. Running one event may
-- trigger others, so we keep going until nothing needs to run
runEvents :: GBState s ()
runEvents = 
    void $ iterateUntil id $ do 
        events <- catMaybes <$> sequence timedEvents    
        cyc    <- readCycles
        let now = filter (\x -> fst x <= cyc) events
        -- run the first event that has triggered, if any
        unless (null now) $ snd (head now)
        -- stop when there are no events to trigger
        return $ null now



updateEvents :: GBState s ()
updateEvents = do
    -- First run any events that might need running
    runEvents

    -- Now figure out the next event (all events are guaranteed to be in the
    -- future)
    events <- catMaybes <$> sequence timedEvents    
    let next      = minimumBy (compare `on` fst) events

    putRef nextEvent $ case events of [] -> Nothing
                                      _  -> Just $ fst next



