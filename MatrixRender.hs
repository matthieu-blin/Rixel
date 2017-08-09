import Haste
import Haste.Graphics.Canvas
import Haste.DOM
import Haste.Events

import Data.IORef
import Data.Word
import Debug.Trace

import Pages
import Control.Exception


--------------------------------------------------------------------------------
-- Pure code, the program logic
-- We follow the philosophy of doing as much as possible in the pure code.

type Size = (Double,Double)

radius :: Double
radius = 15

type Ball  = [Point]
type State = [Ball]

bounce :: Size -> Point -> Int -> Ball
bounce (w,h) (x,y) v
   | v == 0 && y >= maxY = replicate 20 (x,y)
   | y' > maxY           = bounce (w,h) (x,y) (2-v)
   | otherwise           = (x,y) : bounce (w,h) (x,y') v'
 where
   maxY = h-radius
   v'   = v + 1
   y'   = y + fromIntegral v

-- We use Int to represent velocity, because we need to compare it to 0, which
-- is generally a bad idea for floating-point numbers (due to rounding errors).

step :: State -> State
step bs = [ ps | _:ps <- bs ]
  -- Two purposes:
  --
  --  * Drop the first point in each ball
  --  * Filter out finished balls (empty lists)



--------------------------------------------------------------------------------
-- Interactive code
-- This part is only concerned with interaction and drawing, not with the logic
-- determining the behavior of the balls.

ballShape :: Ball -> Shape ()
ballShape []      = return ()
ballShape (pos:_) = circle pos radius

drawBall :: Ball -> Picture ()
drawBall ball = do
    color (RGB 255 0 0) $ fill $ ballShape ball
    stroke $ ballShape ball

animate :: Canvas -> IORef State -> IO ()
animate can state = do
    balls <- readIORef state
    writeIORef state $ step balls
    render can $ mapM_ drawBall balls
    setTimer (Once 20) $ animate can state
    return ()

canWidth, canHeight :: Num a => a
canWidth  = 512
canHeight = 512

nbPixelWidth, nbPixelHeight :: Num a => a
nbPixelWidth = 32
nbPixelHeight = 32

pixelWidth = canWidth / nbPixelWidth
pixelHeight = canHeight / nbPixelHeight

type Palette = [Color]
type Pixel = (Color, Point)
type Pixels = [Pixel]
type PixelMatrix = [Word8]

renderOnePixel :: Pixel -> Picture ()
renderOnePixel p =
  color (pixelcolor) $ fill $ rect (x,y) (x+pixelWidth, y+pixelHeight)
  where pixelpoint = snd p
        x = (fst pixelpoint) * pixelWidth
        y = (snd pixelpoint) * pixelHeight
        pixelcolor = fst p

renderPixel :: Pixels -> Picture ()
renderPixel [] = return ()
renderPixel p = mapM_ renderOnePixel p

palette2Color :: Palette -> Word8 -> Color
palette2Color palette index = if(i < (length palette)) then ( palette !! i) else (RGB 255 255 255)
  where i = fromIntegral index

matrix2Pixel :: [Word8] -> Palette -> Int ->  [Pixel]
matrix2Pixel [] p i = []
matrix2Pixel (w:ws) p i | i >= nbPixelWidth * nbPixelHeight = []
                        | w == 0  = matrix2Pixel ws p (i+1)
                        | otherwise = (matrix2Pixel ws p (i+1)) ++ [(palette2Color p w, index2point i)]

palette2pixel :: [Color] -> Int -> [Pixel]
palette2pixel [] i = []
palette2pixel (w:ws) i = (palette2pixel ws (i+1)) ++ [(w, index2point i)]

currentPalette :: [Color]
currentPalette = [(RGB 0 0 0),(RGB 255 0 0), (RGB 0 255 0), (RGB 0 0 255), (RGB 255 255 0), (RGB 0 255 255), (RGB 255 0 255), (RGB 255 255 255)]

currentMatrix :: [Word8]
currentMatrix  =  replicate (nbPixelWidth * nbPixelHeight) 0

updateN :: [Word8] -> Int -> Word8 -> [Word8]
updateN matrix n paletteIndex = before ++ (paletteIndex:after) where (before, (_:after)) = splitAt n matrix


index2point :: Int -> Point
index2point i = (fromIntegral $ i `mod` nbPixelWidth, fromIntegral $  i `div` nbPixelHeight)

mouse2Index :: (Int ,Int) -> Int
mouse2Index (x,y) = yp * nbPixelWidth + xp
  where xp = x * nbPixelWidth `quot` canWidth
        yp = y * nbPixelHeight `quot` canHeight

renderMatrix :: Canvas -> IORef GameState -> IO ()
renderMatrix canv state = do
  gamestate  <- readIORef state
  render canv $ renderPixel $ matrix2Pixel (matrix gamestate) (palette gamestate) 0
  setTimer (Once 67) $ renderMatrix canv state
  return ()

renderPalette :: Canvas -> IORef GameState -> IO ()
renderPalette canv state = do
  gamestate  <- readIORef state
  render canv $ renderPixel $ palette2pixel (palette gamestate) 0
  setTimer (Once 67) $ renderPalette canv state
  return ()

data GameState = GameState { matrix :: PixelMatrix
                            , palette :: Palette
                            , paletteIndex :: Word8
}

main = do
    canvas <- mkCanvas canWidth canHeight
    canPalette <- mkCanvas canWidth (round pixelHeight)
    clear  <- mkButton "clear"
    changepalette  <- mkButton "change palette"
    column documentBody [canvas,canPalette, clear, changepalette]
    row documentBody [clear, changepalette]

    setStyle documentBody "backgroundColor" "lightblue"
    setStyle documentBody "textAlign" "center"

    Just can <- getCanvas canvas
    Just canp <- getCanvas canPalette

    -- Use an IORef to communicate between the animation and the event handlers
    state <- newIORef $ GameState currentMatrix currentPalette 1

    -- Start the animation
    renderMatrix can state
    renderPalette canp state


    -- Set an event handler for clicks in the canvas
    canvas `onEvent` Click $ \mouse -> do
        let index = mouse2Index $  mouseCoords mouse
        gamestate <- readIORef state
        let currentPaletteIndex = (paletteIndex gamestate)
            newmatrix =  updateN (matrix gamestate) index currentPaletteIndex
            currentPalette = palette gamestate
        writeIORef state $ GameState newmatrix currentPalette currentPaletteIndex

    changepalette `onEvent` Click $ \_ -> do
        gamestate2 <- readIORef state
        let currentPaletteIndex = (paletteIndex gamestate2)
            currentMatrix =  (matrix gamestate2)
            npalette = [(RGB 128 128 128), (RGB 27 32 244), (RGB 188 23 182), (RGB 232 231 12)]
        writeIORef state $ GameState currentMatrix npalette currentPaletteIndex

    -- Set an event handler for the clear button
    clear `onEvent` Click $ \_ -> writeIORef state $ GameState currentMatrix currentPalette 1

-- Note: The current version of Haste does not run the event handler
-- concurrently with the animation, so there's no risk of a race between the
-- different uses of `writeIORef`. If it was truly concurrent, then atomic
-- operations would have to be used instead (see the Data.IORef documentation).
