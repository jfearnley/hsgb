module Gpu (processLine) where

import Data.Word
import Data.List
import Data.Bits
import qualified Data.DList as DList
import Control.Monad
import Control.Monad.RWS.Strict

import Types
import CpuBasic
import Memory
import Util


outputBelowBg :: RenderInstruction -> GBState s ()
outputBelowBg new = 
    modify $ \ out -> out {belowBg = belowBg out `DList.snoc` new }

outputSprite :: RenderInstruction -> GBState s ()
outputSprite new = 
    modify $ \ out -> out {sprites = sprites out `DList.snoc` new }

renderBg :: Word8 -> (Word8, Word8, Int, Int, Maybe Int) -> GBState s ()
renderBg palette (byte1, byte2, toX, toY, clip) = 
        modify $ \ out -> out { background = background out `DList.snoc` new }
    where new = RenderInstruction {
                   renderType = RendBackground,
                   byte1 = byte1, 
                   byte2 = byte2, 
                   palette = palette,
                   toX = toX, 
                   toY = toY,
                   rightClip = clip
                }


---- Loading tile data

lowerTileData :: Word8 -> Word16
lowerTileData tile = 0x8000 + 16 * fromIntegral tile

upperTileData :: Word8 -> Word16
upperTileData tile = 0x9000 + 16 * (fromIntegral $ signed tile)

screenData :: Word16 -> Word8 -> Word8 -> Word16
screenData base tileX tileY = base + 32 * y + x
    where [x, y] = map fromIntegral [tileX, tileY]

lowerTiles = screenData 0x9800
upperTiles = screenData 0x9C00

   
getTile :: (Word8 -> Word16) -> Word8 -> Int -> GBState s (Word8, Word8)
getTile tileAccessor tileIdx offsetY = do
    let lower = tileAccessor tileIdx + 2 * (fromIntegral offsetY)
        upper = lower + 1
    [byte1, byte2] <- mapM readMem [lower, upper]
    return (byte1, byte2)


flipX :: Word8 -> Word8
flipX w = foldr1 (.|.) $ map (f w) [0..7]
    where f x i = if testBit x i then bit (7-i) else 0

flipTile :: (Word8, Word8) -> (Word8, Word8)
flipTile (b1, b2) = (flipX b1, flipX b2)



---- Sprite Rendering

-- Constructs the sprite at OAM location i
makeSprite :: Int -> GBState s Sprite
makeSprite i = do
    let addr = 0xFE00 + fromIntegral i * 4
    [b1, b2, b3, b4] <- mapM readMem [addr..addr+3]
    return Sprite {ypos = fromIntegral b1 - 16, xpos = fromIntegral b2 - 8, tile = b3, sflags = b4}


-- Get all 40 sprites
getSprites :: GBState s [Sprite]
getSprites = do
    dirty <- askRef spriteDirty
    if not dirty then askRef spriteCache
    else do
        sprites <- mapM makeSprite [0..39]
        putRef spriteCache sprites
        putRef spriteDirty False
        return sprites

-- Does the sprite appear on scanline y?
spriteYTest :: Int -> Bool -> Sprite -> Bool
spriteYTest y objSizeBit sprite = 
        sy >= 0 && ((not objSizeBit && sy <= 7) || (objSizeBit && sy <= 15))
    where sy = y - ypos sprite


-- Get the sprites on line y, split into those behind the background, and those
-- above
spritesOnLine :: Int -> Bool -> GBState s ([Sprite], [Sprite])
spritesOnLine y objSizeBit = do
    sprites <- getSprites
    let visible = filter (spriteYTest y objSizeBit) sprites
        sorted  = sortOn (\s -> - xpos s) visible
        layer s = testBit (sflags s) 7
    return $ partition layer sorted


renderSprite :: Int -> Bool -> (RenderInstruction -> GBState s ()) -> Sprite -> GBState s ()
renderSprite y objSizeBit output s = do
    let useOBP1  = testBit (sflags s) 4
        flipx    = testBit (sflags s) 5
        flipy    = testBit (sflags s) 6
        sy       = y - ypos s
        tsy      = case (flipy, objSizeBit) of
                            (True, False) ->  7 - sy 
                            (True, True)  -> 15 - sy
                            (_, _)        -> sy
        tileIdx  = tile s + if tsy <= 7 then 0 else 1
        tileLine = tsy `mod` 8
    (b1, b2) <- getTile lowerTileData tileIdx tileLine

    palette  <- if useOBP1 then readOBP1 else readOBP0
    let (b1'', b2'') = (if flipx then flipTile else id) (b1, b2)

    output $ RenderInstruction {
                renderType = RendSprite, 
                byte1 = b1'', 
                byte2 = b2'', 
                palette = palette, 
                toX = xpos s, 
                toY = y,
                rightClip = Nothing
            }



---- Background rendering


-- The x-coordinates of the tiles that need to be drawn to the screen for a
-- given screen offset, taking into account the position of the window
tileLocs :: Word8 -> Maybe Int -> [(Word8, Int, Maybe Int)]
tileLocs scx wx = 
    let
        offset = fromIntegral $ scx `mod` 8
        base   = fromIntegral $ scx `div` 8
        upper  = base + if offset == 0 then 19 else 20
        rawTiles = [(fromIntegral $ i `mod` 32, -- Tile number
                     8 * (i - base) - offset)   -- x position
                        | i <- [base .. upper]]
    in
        clipTiles wx rawTiles
        

clipTiles :: Maybe Int -> [(Word8, Int)] -> [(Word8, Int, Maybe Int)]
clipTiles Nothing   rawTiles = map (\ (x, y) -> (x, y, Nothing)) rawTiles
clipTiles (Just wx) rawTiles = 
    let
        noWindow = filter (\ (_, x) -> x < wx) rawTiles
        clipf (t, x) = (t, x, if x + 8 >= wx then Just (wx - x) 
                                             else Nothing)
    in
        map clipf noWindow
        


getBGTile :: (Word8 -> Word8 -> Word16) -> (Word8 -> Word16) 
                -> Word8 -> Word8 -> GBState s (Word8, Word8)
getBGTile screenData tileData y tileX = do
    let (tileY, offsetY) = divMod y 8
        addr = screenData tileX tileY
    tile <- readMem addr
    getTile tileData tile (fromIntegral offsetY)


renderBackground :: Int -> GBState s ()
renderBackground screenY = do
    -- Data sources
    tileMapBit  <- regBit readLCDC 3
    tileDataBit <- regBit readLCDC 4
    let screenData  = if tileMapBit  then upperTiles    else lowerTiles
        tileData    = if tileDataBit then lowerTileData else upperTileData


    -- Screen scrolling and window
    scx <- readSCX
    scy <- readSCY 
    actualWX <- windowX screenY
    let bgY = fromIntegral $ (screenY + fromIntegral scy) `mod` 256
        (tileXs, xOffsets, clips) = unzip3 $ tileLocs scx actualWX
       
    -- Produce tile data
    tileData <- mapM (getBGTile screenData tileData bgY) tileXs
    let (byte1s, byte2s) = unzip tileData

    -- Create render instructions
    bgp <- readBGP
    let outputs = zip5 byte1s byte2s xOffsets (repeat screenY) clips
    mapM_ (renderBg bgp) outputs


---- Window rendering

-- Is the window enabled on this scanline? If so, where does it start?
windowX :: Int -> GBState s (Maybe Int)
windowX screenY = do
    windowEnabled <- regBit readLCDC 5
    wx <- readWX
    wy <- readWY
    return $ if | not windowEnabled         -> Nothing
                | screenY < fromIntegral wy -> Nothing 
                | wx > 166                  -> Nothing
                | otherwise -> Just (fromIntegral wx - 7)


renderWindow :: Int -> GBState s ()
renderWindow screenY = do
    wxEnabled <- windowX screenY
    case wxEnabled of 
        Nothing -> return ()
        Just wx -> do
            -- Data sources
            tileMapBit  <- regBit readLCDC 6
            tileDataBit <- regBit readLCDC 4
            let screenData  = if tileMapBit  then upperTiles    else lowerTiles
                tileData    = if tileDataBit then lowerTileData else upperTileData

            -- Get Tiles
            let wLength = 160 - wx
            let nTiles  = (wLength `div` 8) + 
                            if (wLength `mod` 8) /= 0 then 1 else 0
                tileIdxs = [0 .. fromIntegral nTiles]
                xOffsets = [wx + fromIntegral x * 8 | x <- tileIdxs]
            y <- askRef windowLine
            tileData <- mapM (getBGTile screenData tileData y) tileIdxs

            -- Create render instructions
            let (byte1s, byte2s) = unzip tileData
            bgp <- readBGP
            let outputs = zip5 byte1s byte2s xOffsets (repeat screenY) (repeat Nothing)
            mapM_ (renderBg bgp) outputs

            modifyRef windowLine (+1)


---- The main rendering function: render all pixels on line y

processLine :: Int -> GBState s ()
processLine y = do
    lcdc <- readLCDC

    -- Find the sprites that need to be rendered
    let enabled      = testBit lcdc 7
        objSizeBit   = testBit lcdc 2
        objRenderBit = testBit lcdc 1
        priorityBit  = testBit lcdc 0
    (lowerSprites, upperSprites) <- spritesOnLine y objSizeBit

    when objRenderBit $ do
        mapM_ (renderSprite y objSizeBit outputBelowBg) lowerSprites
    when (enabled && priorityBit) $ do 
        renderBackground y
        renderWindow y
    when objRenderBit $ do
        mapM_ (renderSprite y objSizeBit outputSprite)  upperSprites



