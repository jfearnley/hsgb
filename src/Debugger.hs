module Debugger where

import System.IO
import System.Console.Repline
import System.Console.Pretty
import System.FilePath
import System.Directory
import Control.Applicative
import Control.Monad.Loops
import Control.Monad.State.Strict
import Data.List
import Data.Word
import Data.Maybe
import Data.Functor.Identity
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Bimap as Bimap
import Text.Read (readMaybe)


import Types
import GB
import qualified Util 
import Memory
import CpuBasic
import Emulation
import Disassembler

-- Basic interaction with the repl monad

type Trigger = Map.Map Word16 (Maybe Word8)

data DebugState = DebugState {
    gameboy :: PureGB,
    symbols :: Symbols,
    filename :: String,
    breakpoints :: Set.Set Word16,
    triggers :: Trigger
}

type Repl a = HaskelineT (StateT DebugState IO) a

putStrC :: String -> Repl () 
putStrC = liftIO . putStr 

putStrLnC :: String -> Repl () 
putStrLnC = liftIO . putStrLn

printC :: Show a => a -> Repl ()
printC = liftIO . print 

getAddr :: String -> Repl (Maybe Word16)
getAddr symbol = do
    symbs <- gets symbols
    return $ Bimap.lookup symbol symbs

getSymb :: MonadState DebugState m => Word16 -> m (Maybe String)
getSymb addr = do
    symbs <- gets symbols
    return $ Bimap.lookupR addr symbs

-- Parsing and showing addresses

parseByte :: String -> Maybe Word8
parseByte str = readMaybe $ "0x" ++ str

parseAddr :: String -> Repl (Maybe Word16)
parseAddr str = do
    symb <- getAddr str
    let prefixed = (if "0x" `isPrefixOf` str then "" else "0x") ++ str
        addr = readMaybe prefixed :: Maybe Word16
    return $ symb <|> addr

showAddr :: MonadState DebugState m => Word16 -> m String
showAddr addr = do
    symb <- getSymb addr
    return $ fromMaybe (toHex16 addr) symb

toHex :: Word8 -> String
toHex x = "0x" ++ Util.toHex x

toHex16 :: Word16 -> String
toHex16 x = "0x" ++ Util.toHex16 x

toHex16A :: DebugState -> Word16 -> String
toHex16A state addr = case Bimap.lookupR addr (symbols state) of 
                            Just x  -> x ++ "(" ++ toHex16 addr ++ ")"
                            Nothing -> toHex16 addr

-- Load roms and symbols

type Symbols = Bimap.Bimap String Word16

parseSymbolLine :: String -> Maybe (String, Word16)
parseSymbolLine s = 
    let
        w = words s
        addrS = drop 3 $ head w
        addr  = readMaybe ("0x" ++ addrS) :: Maybe Word16
    in
        case (w, addr) of
            ([_, x], Just addr) -> Just (x, addr)
            _                   -> Nothing

loadSymbols :: String -> IO Symbols
loadSymbols romFile = do
    let symbFile = replaceExtension romFile ".sym"
    exists <- doesFileExist symbFile
    if not exists
    then return Bimap.empty
    else do
        symbolData <- readFile symbFile
        let symLines = lines symbolData
            parsed   = mapMaybe parseSymbolLine symLines
            bimap    = Bimap.fromList parsed
        return bimap

loadState :: String -> IO (Maybe DebugState)
loadState file = do
        let fname = takeFileName file
        exists <- doesFileExist file
        if not exists then return Nothing
        else do 
            mgb <- loadRom file
            let gb = fromMaybe (error "Error: only MBC1 roms are supported") mgb
            putStr $ "\n" ++ color Blue (style Bold fname) ++ " loaded\n"

            symbols <- loadSymbols file
            putStrLn $ show (Bimap.size symbols) ++ " symbols loaded"
            putStr "\n"

            printRomStats gb
            putStr "\n"
            return $ Just DebugState 
                        {
                            gameboy = gb, 
                            symbols = symbols, 
                            filename = fname,
                            breakpoints = Set.empty,
                            triggers = Map.empty
                        }

makeState :: PureGB -> DebugState
makeState gb = 
    DebugState
        {
            gameboy = gb, 
            symbols = Bimap.empty,
            filename = "",
            breakpoints = Set.empty,
            triggers = Map.empty
        }
    

load :: [String] -> Repl ()
load [x] = do
    state <- liftIO $ loadState x
    case state of
        Nothing -> putStrLnC $ "failed to load " ++ x
        Just st -> put st
load _ = putStrLnC "usage: load [filename]"


-- Generic code for a one argument command that takes an address

oneAddress :: String -> (Word16 -> Repl ()) -> [String] -> Repl ()
oneAddress _ action [x] = do
    addr <- parseAddr x
    case addr of 
        Nothing   -> putStrLnC "bad address or unknown label"
        Just addr -> action addr
oneAddress name _ _ = putStrLnC $ "usage: " ++ name ++ " [address]"

-- Generic code for a two argument command that takes an address and a value

addressVal :: String -> (Word16 -> String -> Repl ()) -> [String] -> Repl ()
addressVal _ action [x, y] = do
    addr <- parseAddr x
    case addr of
        Nothing   -> putStrLnC "bad address or unknown label"
        Just addr -> action addr y
addressVal name _ _ = putStrLnC $ "usage: " ++ name ++ " [address] [value]"

-- Printing memory

printMem :: [String] -> Repl ()
printMem = oneAddress "print" $ \addr -> do
    gb <- gets gameboy
    let val = GB.evalState (readMem addr) gb
    putStrLnC $ "mem[" ++ toHex16 addr ++ "] = " ++ toHex val


-- Printing a disassebmly

disassembly' :: PureGB -> Symbols -> Int -> Word16 -> Repl()
disassembly' _  _     0 _ = return ()
disassembly' gb symbs i addr = do
    let (instr, l) = decodeGBSymb symbs gb addr
        prefix     = if askPure regPC gb == addr 
                    then (style Bold "-->     ") 
                    else "        "
    case Bimap.lookupR addr symbs of 
        Just x  -> putStrLnC $ color Yellow $ x ++ ":"
        Nothing -> return ()
    putStrLnC $ prefix ++ show instr
    unless (unconditionalBranch instr) $ 
        disassembly' gb symbs (i-1) (addr + fromIntegral l)

disassembly :: Repl()
disassembly = do
    gb    <- gets gameboy
    symbs <- gets symbols
    disassembly' gb symbs 10 $ askPure regPC gb


-- Breakpoints

setBP :: [String] -> Repl ()
setBP = oneAddress "breakpoint" $ \addr -> do
    bps <- gets breakpoints
    if addr `Set.member` bps 
        then putStrLnC $ toHex16 addr ++ " is already a breakpoint"
        else do
            putStrLnC $ "breakpoint set at " ++ toHex16 addr
            modify $ \ s -> s {breakpoints = Set.insert addr bps }


delBP :: [String] -> Repl ()
delBP = oneAddress "delete breakpoint" $ \addr -> do
    bps <- gets breakpoints
    if addr `Set.notMember` bps 
        then putStrLnC $ toHex16 addr ++ " is not a breakpoint"
        else do
            putStrLnC $ "breakpoint at " ++ toHex16 addr ++ " deleted"
            modify $ \ s -> s {breakpoints = Set.delete addr bps }

-- Triggers

setTrigger :: [String] -> Repl ()
setTrigger = addressVal "trigger" $ \addr val -> do
    trigs <- gets triggers
    let parsed = parseByte val
    if | addr `Map.member` trigs -> putStrLnC $ toHex16 addr ++ " is already a trigger target"
       | isNothing parsed && val /= "any" -> putStrLnC $ show val ++ " is not a valid trigger value"
       | otherwise -> modify $ \ s -> s {triggers = Map.insert addr parsed trigs}


delTrigger :: [String] -> Repl ()
delTrigger = oneAddress "delete trigger" $ \addr -> do
    return ()
    trigs <- gets triggers
    if addr `Map.notMember` trigs
        then putStrLnC $ toHex16 addr ++ " is not a trigger target"
        else do
            putStrLnC $ "trigger for " ++ toHex16 addr ++ " deleted"
            modify $ \ s -> s {triggers = Map.delete addr trigs }

-- Delete

deleteArgs = ["breakpoint", "trigger"]

deleteCmd :: [String] -> Repl ()
deleteCmd ("breakpoint":xs) = delBP xs
deleteCmd ("trigger":xs)    = delTrigger xs
deleteCmd _ = do
    putStrLnC "usage: show [argument]\n"
    putStrLnC "possible arguments:"
    mapM_ (\x -> putStrLnC $ "\tdelete " ++ x) deleteArgs

-- Step

nextCmd :: [String] -> Repl ()
nextCmd [] = do
    gb <- gets gameboy
    let gb'    = GB.execState step gb
    modify $ \s -> s{gameboy = gb'}
    status []
nextCmd _ = putStrLnC "usage: next [#nsteps]"


-- Show

showArgs = ["breakpoints", "triggers"]

showCmd :: [String] -> Repl ()
showCmd ["breakpoints"] = do
    bps <- gets breakpoints
    mapM_ printC $ Set.toList bps
showCmd ["triggers"] = do
    triggers <- gets triggers
    let showf (x, y) = do
            addr <- showAddr x
            putStrLnC $ addr ++ ": " ++ case y of Just z  -> toHex z
                                                  Nothing -> "any"
    mapM_ showf (Map.toList triggers)
showCmd _ = do
    putStrLnC "usage: show [argument]\n"
    putStrLnC "possible arguments:"
    mapM_ (\x -> putStrLnC $ "\tshow " ++ x) showArgs

-- Status

statusArgs = ["registers", "flags", "interrupts", "gpu", "disassembly"]

status :: [String] -> Repl ()
status [] = do
    putStrLnC ""
    status ["registers"]
    status ["flags"]
    putStrLnC ""
    status ["disassembly"]
    putStrLnC ""
status ["registers"] = do
    gb <- gets gameboy
    putStrC $ showRegs gb
status ["flags"] = do
    gb <- gets gameboy
    putStrC $ showFlags gb
status ["interrupts"] = do
    gb <- gets gameboy
    putStrC $ showInterrupts gb
status ["disassembly"] = disassembly
status ["gpu"] = do
    gb <- gets gameboy
    putStrC $ showGPU gb
status _ = do
    putStrLnC "usage: status [argument]\n"
    putStrLnC "possible arguments:"
    mapM_ (\x -> putStrLnC $ "\tshow " ++ x) statusArgs

-- Run


stopBP :: DebugState -> GBState s (Maybe String)
stopBP state = do
    pc <- askRef regPC
    return $ if pc `Set.member` breakpoints state
                then Just $ "breakpoint at " ++ toHex16A state pc ++ " encountered"
                else Nothing


stopTriggerVal :: DebugState -> Word16 -> Word8 -> GBState s (Maybe String)
stopTriggerVal state addr trig = do
    val <- readMem addr
    return $ if val == trig
        then Just $ "trigger: value at " ++ toHex16A state addr ++ " = " ++ show trig
        else Nothing

stopTriggerAll :: DebugState -> Word16 -> Word8 -> GBState s (Maybe String)
stopTriggerAll state addr orig = do
    val <- readMem addr
    return $ if val /= orig
        then Just $ "trigger: value at " ++ toHex16A state addr ++ " has changed (" ++ show orig ++ " -> " ++ show val ++ ")"
        else Nothing

stopTriggers :: DebugState -> GBState s [Maybe String]
stopTriggers state = do
    let trigs  = triggers state 
        f (addr, target) = case target of 
                            Just y  -> stopTriggerVal state addr y
                            Nothing -> stopTriggerAll state addr $
                                   GB.evalState (readMem addr) (gameboy state)
    mapM f (Map.toList trigs)

stop :: DebugState -> GBState s [String]
stop state = do
    bps   <- stopBP state
    trigs <- stopTriggers state

    return $ catMaybes (bps : trigs)

stopCond :: DebugState -> GBState s Bool
stopCond state = fmap (not . null) (stop state)



run :: [String] -> Repl ()
run [] = do
    state <- get
    let gb'    = GB.execState (untilM_ step (stopCond state)) (gameboy state)
        reason = GB.evalState (stop state) gb'
    modify $ \s -> s{gameboy = gb'}

    mapM_ putStrLnC reason
    status []
run _ = putStrLnC "usage: run"


-- Log to file

logLine :: PureGB -> Instruction -> String
logLine gb instr = 
    let
        instrStr = showInstr (\x -> x ++ replicate (4 - length x) ' ') instr

        -- 8 bit registers
        reg8 name reg = name ++ ":" ++ (drop 2 $ toHex $ askPure reg gb)
        regs = map (uncurry reg8) [("A", regA), ("B", regB), ("C",regC), ("D",regD), ("E",regE), ("H",regH), ("L",regL)]

        -- 16 bit registers
        reg16 name reg = name ++ ":" ++ (drop 2 $ toHex16 $ askPure reg gb)

        -- Flags
        flagf name reg = if fs then name else ' '
            where fs = askPure reg gb
        flags = map (uncurry flagf) [('z', zero), ('n', negative), ('h', hcarry), ('c', carry)]
--         flags = flagf "z" zero
    in
        unwords $ [reg16 "PC" regPC] ++ regs ++ [reg16 "SP" regSP, flags, instrStr]

runLogger :: Handle -> Integer -> Repl ()
runLogger _      0 = return ()
runLogger handle n = do
    state <- get
    let gb = GB.execState step (gameboy state)
    modify $ \s -> s{gameboy = gb}

    let instr = fst $ decodeGB gb $ askPure regPC gb
        line = logLine gb instr
    liftIO $ hPutStrLn handle line
    runLogger handle (n-1)



logger :: [String] -> Repl ()
logger [x] = do
    handle <- liftIO $ openFile x WriteMode
    runLogger handle 1000000
    liftIO $ hClose handle
logger _ = putStrLnC "usage: log [filename]"


-- Send a key to the emulator

sendKey :: [String] -> Repl ()
sendKey [x] = do
    case (readMaybe x) :: Maybe Key of 
        Nothing  -> return ()
        Just key -> do
            gb <- gets gameboy
            let newKeys = Set.insert key (runIdentity . keys $ gb)
                gb'     = gb {keys = Identity newKeys}
            modify $ \s -> s {gameboy = gb'}
            putStrLnC $ "pushed " ++ show key
sendKey _ = putStrLnC "usage: key [filename]"



-- Commands in the repl

commands :: [(String, [String] -> Repl ())]
commands = [
                ("print", printMem),
                ("load", load),
                ("breakpoint", setBP),
                ("trigger", setTrigger),
                ("show", showCmd),
                ("next", nextCmd),
                ("status", status),
                ("delete", deleteCmd),
                ("run", run),
                ("log", logger),
                ("key", sendKey)
           ]

cmd :: String -> Repl ()
cmd str = 
    let
        w   = words str
        cmd = head w
        cands = filter (\ x -> cmd `isPrefixOf` fst x) commands 
    in
        unless (null w) $ case cands of
            []             -> putStrLnC $ "unknown command: " ++ str
            [(_, command)] -> command $ tail (words str)
            list           -> do
                putStrLnC $ "no command " ++ show str
                putStrLnC $ "    did you mean: " ++ unwords (map fst list) ++ "?"


-- Tab completion
firstWord :: Monad m => WordCompleter m
firstWord n = return $ filter (isPrefixOf n) names
    where names = map fst commands

symbolCompleter :: (Monad m, MonadState DebugState m) => CompletionFunc m
symbolCompleter arg = do
    symbs <- gets symbols
    let symbNames = map fst $ Bimap.toList symbs
    listCompleter symbNames arg

breakpointCompleter :: (Monad m, MonadState DebugState m) => CompletionFunc m
breakpointCompleter arg = do
    bps     <- gets breakpoints
    bpNames <- mapM showAddr $ Set.toList bps
    listCompleter bpNames arg

keyCompleter :: (Monad m, MonadState DebugState m) => CompletionFunc m
keyCompleter arg = listCompleter keyStrings arg
    where keys = [minBound .. maxBound] :: [Key]
          keyStrings = map show keys


prefixedMatcher :: (MonadIO m, MonadState DebugState m) => [(String, CompletionFunc m)]
prefixedMatcher = [
    ("load", fileCompleter),
    ("print", symbolCompleter),
    ("breakpoint", symbolCompleter),
    ("trigger", symbolCompleter),
    ("rmtrigger", symbolCompleter),
    ("show", listCompleter showArgs),
    ("status", listCompleter statusArgs),
    ("delete breakpoint", breakpointCompleter),
    ("delete trigger", symbolCompleter),
    ("delete", listCompleter deleteArgs),
    ("key", keyCompleter)
  ]


-- The repl

banner :: Repl String
banner = do
    fname  <- gets filename
    gb     <- gets gameboy
    pcaddr <- showAddr $ askPure regPC gb
    let filestr = color Blue $ style Bold fname
        pcstr   = color Green $ style Bold $ "[" ++ pcaddr ++ "]"
        cursor  = style Bold "> "
    return $  filestr ++ pcstr ++ cursor


repl :: DebugState -> IO ()    
repl state = flip evalStateT state
    $ evalRepl banner 
                cmd 
                [] 
                Nothing 
                (Prefix (wordCompleter firstWord) prefixedMatcher)
                (return ())

launchDebugger :: PureGB -> IO ()
launchDebugger = repl . makeState
