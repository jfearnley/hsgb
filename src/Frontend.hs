module Frontend (doFrontend, initFrontend, quitFrontend, everySecond) where

import SDL
import SDL.Internal.Types
import SDL.Internal.Exception
import qualified SDL.Raw.Video as RawVideo
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.C.Types
import Data.Word
import qualified Data.List as List
import qualified Data.DList as DList
import Data.List.Index 
import Data.Bits
import Data.Maybe
import Data.Functor.Identity 
import Data.Text (pack)
import Data.IORef
import Text.Printf
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.HashTable.IO as HT
import Control.Concurrent
import Control.Monad
import qualified Data.ListLike as LL
import Debug.Trace

import Types
import GB



-- Parse keyboard events into a KeySet

keymap :: [(Keycode, Key)]
keymap = [ 
            (KeycodeUp, KeyUp),
            (KeycodeDown, KeyDown),
            (KeycodeLeft, KeyLeft),
            (KeycodeRight, KeyRight),
            (KeycodeZ, KeyB),
            (KeycodeX, KeyA),
            (KeycodeA, KeySelect),
            (KeycodeS, KeyStart),
            (KeycodeB, KeyDebug),
            (KeycodeEscape, KeyQuit)
         ]

decodeKey :: Event -> Maybe (Key, InputMotion)
decodeKey event = 
    case eventPayload event of
        KeyboardEvent ke -> 
            do  key <- lookup (keysymKeycode (keyboardEventKeysym ke)) keymap
                return $ (key, keyboardEventKeyMotion ke)
        _                -> Nothing


updateKeys :: KeySet -> Event -> KeySet
updateKeys keys event = 
    case decodeKey event of
        Just (key, Pressed)  -> Set.insert key keys
        Just (key, Released) -> Set.delete key keys
        _                    -> keys

parseEvents :: KeySet -> [Event] -> KeySet
parseEvents keys events = List.foldl' updateKeys keys events

-- Initialize SDL

initFrontend :: IO (Frontend)
initFrontend = do
    initializeAll
    window <- createWindow (pack "hsgb") (defaultWindow {windowInitialSize = V2 (4*160) (4*144)})

    -- Renderer
    renderer <- createRenderer window (-1) defaultRenderer
    rendererScale         renderer $= 4
    rendererDrawColor     renderer $= V4 255 255 255 255
    rendererDrawBlendMode renderer $= BlendAlphaBlend
    clear   renderer 
    present renderer

    -- Audio
    audioBufferL <- newIORef (DList.empty)
    audioBufferR <- newIORef (DList.empty)
    cbBufferL    <- newIORef (VU.empty)
    cbBufferR    <- newIORef (VU.empty)
    let cbData = CallbackData audioBufferL audioBufferR cbBufferL cbBufferR
        spec   = OpenDeviceSpec
                 {
                     openDeviceFreq     = Mandate 48000,
                     openDeviceFormat   = Mandate Unsigned8BitAudio,
                     openDeviceChannels = Mandate Stereo,
                     openDeviceSamples  = 1024,
                     openDeviceCallback = audioCallback cbData,
                     openDeviceUsage    = ForPlayback,
                     openDeviceName     = Nothing
                 }
    (device, _) <- openAudioDevice spec
    setAudioDevicePlaybackState device Play

    -- Initialize Frontend data structure
    emptyHT  <- HT.new
    emptyHT2 <- HT.new
    feTime   <- time :: IO Double
    initTime <- ticks
    delay    <- newIORef 0
    return $ Frontend 
        {
            renderer      = renderer,
            audioDevice   = device,
            window        = window,
            textureCache  = emptyHT,
            textureCache2 = emptyHT2,
            audioBufferL  = audioBufferL,
            audioBufferR  = audioBufferR,
            feTimer       = feTime,
            initTime      = initTime,
            cpuDelayTime  = delay
        }

quitFrontend :: Frontend -> IO ()
quitFrontend fe = do
    setAudioDevicePlaybackState (audioDevice fe) Pause
    closeAudioDevice (audioDevice fe) 
    
wait :: PureGB -> Frontend -> IO ()
wait gb fe = do
    time <- fromIntegral <$> ticks
    let Identity frame = fromIntegral $ frameCount gb
        start    = fromIntegral $ initTime fe
        expected = 1000 * start + frame * 16743 -- 59.73 FPS
        delay    = max (expected - 1000 * time) 0
    when (delay > 0) $ do
        modifyIORef' (cpuDelayTime fe) (+delay) 
        threadDelay delay

doFrontend :: Frontend -> PureGB -> EmulatorOutput -> IO (KeySet)
doFrontend fe gb output = do
    doAudio fe output
    renderFrame fe output
    wait gb fe
    events <- pollEvents
    return $ parseEvents (askPure keys gb) events


everySecond :: Frontend -> IO (Frontend)
everySecond fe = do
    emptyHT <- HT.new

    -- How much cpu usage are we using?
    delay <- readIORef $ cpuDelayTime fe
    writeIORef (cpuDelayTime fe) 0
    let second = 16743*60
        cpu    = 100 * (second - fromIntegral delay) / second :: Double
        cpuS   = printf "%.0f" cpu :: String

    -- What is our FPS?
    now <- time :: IO Double
    let last = feTimer fe
        fps  = 60 / (now - last)
        fpsS = printf "%.0f" fps :: String


    let title   = pack $ ("hsgb (" ++ fpsS ++ " FPS, " ++ cpuS ++ "% CPU)")
    windowTitle (window fe) $= title


    return $ fe {
                    textureCache2 = textureCache fe, 
                    textureCache = emptyHT,
                    feTimer = now
                }

---- Rendering

-- Gameboy colors to RGBA8888 (little endian)
colorToRGBA8888 :: Int -> [Word8]
colorToRGBA8888 col
    | col == 0  = [255, 255, 255, 255]
    | col == 1  = [255, 170, 170, 170]
    | col == 2  = [255,  85,  85,  85]
    | col == 3  = [255,   0,   0,   0]
    | otherwise = error "impossible color passed to colorTORGBA8888"

transparent :: [Word8]
transparent = [0, 0, 0, 0]

-- Decode a gameboy tile line
decodeBit :: RenderInstruction -> Int -> [Word8]
decodeBit ri i = 
    let
        -- Convert two booleans to a 2 bit number
        b x = if x then 1 else 0
        twoB x y = b x + 2 * b y

        -- Decode the color
        rawCol  = twoB (testBit (byte1 ri) i) (testBit (byte2 ri) i)
        pIdx    = rawCol * 2
        pal     = palette ri
        realCol = twoB (testBit pal pIdx) (testBit pal $ pIdx + 1)
    in
        if | renderType ri == RendBackground && realCol == 0 -> transparent
           | renderType ri == RendSprite     && rawCol  == 0 -> transparent
           | otherwise -> colorToRGBA8888 realCol

decode' :: RenderInstruction -> VU.Vector Word8
decode' ri = VU.fromList $ concatMap process (reverse [0..7])
    where process x = decodeBit ri x

-- Decoding gameboy tiles is expensive, so we cache the decoded pixels
riToCacheKey :: RenderInstruction -> (Bool, Word8, Word8, Word8)
riToCacheKey ri = (renderType ri == RendBackground, 
                        byte1 ri, byte2 ri, palette ri)

cacheLookup :: Frontend -> RenderInstruction -> IO (Maybe (VU.Vector Word8))
cacheLookup fe ri = do
    let key = riToCacheKey ri
    cache1   <- HT.lookup (textureCache fe) key
    case cache1 of 
        Just x  -> return $ Just x
        Nothing -> do
            cache2 <- HT.lookup (textureCache2 fe) key
            case cache2 of (Just y) -> HT.insert (textureCache fe) key y
                           Nothing  -> return ()
            return cache2


decode :: Frontend -> RenderInstruction -> IO (VU.Vector Word8)
decode fe ri = do
    cached <- cacheLookup fe ri
    case cached of 
        Just x  -> return x
        Nothing -> do
            let decoded = decode' ri
                key     = riToCacheKey ri
            HT.insert (textureCache fe) key decoded
            return decoded


-- Copying pixel data to the screen

copyPixel :: VSM.IOVector Word8 -> VU.Vector Word8 -> Int -> Int -> Int -> IO ()
copyPixel target source x y idx 
    | x < 0 || x >= 160          = return () -- pixel is off screen
    | source VU.! (idx * 4) == 0 = return () -- pixel is transparent
    | otherwise = mapM_ cp [1..3]
    where base = y * 160 * 4 + x * 4
          cp i = VSM.write target (base + i) (source VU.! (idx * 4 + i))

renderRI :: Frontend -> VSM.IOVector Word8 -> RenderInstruction -> IO ()
renderRI fe pixelData ri = do
    riData <- decode fe ri
    let n = fromMaybe 8 $ rightClip ri
    mapM_ (\x -> copyPixel pixelData riData (toX ri + x) (toY ri) x) [0..n-1]


renderFrame :: Frontend -> EmulatorOutput -> IO ()
renderFrame fe output = do
    let rend = renderer fe

    pixelData <- VSM.new (4 * 160 * 144)
    VSM.set pixelData 255

    let instructions = DList.concat [belowBg output, background output, sprites output]
    mapM_ (renderRI fe pixelData) instructions

    surface <- createRGBSurfaceFrom pixelData (V2 160 144) (4 * 160) RGBA8888 
    texture <- createTextureFromSurface rend surface
    copy rend texture Nothing Nothing
    destroyTexture texture
    present rend


---- Audio

doAudio :: Frontend -> EmulatorOutput -> IO ()
doAudio fe output = do
    setAudioDeviceLocked (audioDevice fe) Locked
    modifyIORef' (audioBufferL fe) $ \ b -> b <> soundL output
    modifyIORef' (audioBufferR fe) $ \ b -> b <> soundR output
    setAudioDeviceLocked (audioDevice fe) Unlocked



flattenOutput :: SampleList -> Samples
flattenOutput = VU.concat . DList.toList 


data CallbackData = CallbackData
    {
        emuBufferL    :: IORef SampleList,
        emuBufferR    :: IORef SampleList,
        sampleBufferL :: IORef Samples,
        sampleBufferR :: IORef Samples
    }


getAudio :: IORef SampleList -> IORef Samples -> Int -> IO Samples
getAudio emuBuffer sampleBuffer nSamples = do
    emuOutput <- readIORef emuBuffer 
    buffered  <- readIORef sampleBuffer 

    let flattened = flattenOutput $ buffered `DList.cons` emuOutput
        (now, later) = VU.splitAt nSamples flattened

    writeIORef emuBuffer    DList.empty
    writeIORef sampleBuffer later

    return now

audioCallback :: CallbackData -> AudioFormat a -> VSM.IOVector a -> IO ()
audioCallback cbData Unsigned8BitAudio outVector = do
    -- Get data
    let nSamples = (VSM.length outVector) `div` 2
    audioL <- getAudio (emuBufferL cbData) (sampleBufferL cbData) nSamples
    audioR <- getAudio (emuBufferR cbData) (sampleBufferR cbData) nSamples

    -- Write data in LRLRLR format
    let sRange = [0..nSamples-1] 
        getData buffer i = 128 + if i < VU.length buffer 
                                 then buffer VU.! i 
                                 else 0 
    mapM_ (\ i -> VSM.write outVector (2*i)   (getData audioL i)) sRange
    mapM_ (\ i -> VSM.write outVector (2*i+1) (getData audioR i)) sRange
audioCallback cbData FloatingLEAudio outVector = do
    -- Get data
    let nSamples = (VSM.length outVector) `div` 2
    audioL <- getAudio (emuBufferL cbData) (sampleBufferL cbData) nSamples
    audioR <- getAudio (emuBufferR cbData) (sampleBufferR cbData) nSamples

    -- Write data in LRLRLR format
    let sRange = [0..nSamples-1] 
        getData buffer i = if i < VU.length buffer 
                                 then (fromIntegral $ (buffer VU.! i)) /256
                                 else 0 
    mapM_ (\ i -> VSM.write outVector (2*i)   (getData audioL i)) sRange
    mapM_ (\ i -> VSM.write outVector (2*i+1) (getData audioR i)) sRange



audioCallback _ a _ = error $ "unsupported audio format: " ++ show a

