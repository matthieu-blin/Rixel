import Haste
import Haste.Graphics.Canvas
import Haste.DOM
import Haste.Events

import Data.IORef
import Data.Word
import Debug.Trace
import qualified Data.Vector.Unboxed as V

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

matrix2Pixel :: [Word8] -> Palette -> Int -> Int ->  [Pixel]
matrix2Pixel [] p x y = []  
matrix2Pixel (w:ws) p x y | x < nbPixelWidth-1 = (matrix2Pixel ws p (x+1) y) ++ [(palette2Color p w, (fromIntegral x, fromIntegral y))] 
                          | y < nbPixelHeight-1= (matrix2Pixel ws p 0 (y+1)) ++ [(palette2Color p w, (fromIntegral x, fromIntegral y))] 
                          | otherwise = [(palette2Color p w, (fromIntegral x, fromIntegral y))] 

currentPalette :: [Color]
currentPalette = [(RGB 0 0 0),(RGB 255 0 0), (RGB 0 255 0), (RGB 0 0 255), (RGB 255 255 0), (RGB 0 255 255), (RGB 255 0 255), (RGB 255 255 255)]

currentMatrix :: [Word8]
currentMatrix  =  replicate (nbPixelWidth * nbPixelHeight) 0

updateN :: [Word8] -> Int -> Word8 -> [Word8]
updateN matrix n paletteIndex = before ++ (paletteIndex:after) where (before, (_:after)) = splitAt n matrix
            

pos2Index :: (Int, Int) -> Int
pos2Index (x,y) = y* nbPixelWidth + x

mouse2pos :: (Int ,Int) -> (Int, Int)
mouse2pos (x,y) = (x * nbPixelWidth `quot` canWidth,y * nbPixelHeight `quot` canHeight) 

renderMatrix :: Canvas -> IORef GameState -> IO ()
renderMatrix canv state = do
  gamestate  <- readIORef state
  render canv $ renderPixel $ matrix2Pixel (matrix gamestate) (palette gamestate) 0 0
  setTimer (Once 67) $ renderMatrix canv state
  return ()
  
data GameState = GameState { matrix :: PixelMatrix
                            , palette :: Palette
}

main = do
    canvas <- mkCanvas canWidth canHeight
    clear  <- mkButton "clear"
    changepalette  <- mkButton "change palette"
    column documentBody [canvas,clear, changepalette]
    row documentBody [clear, changepalette]

    setStyle documentBody "backgroundColor" "lightblue"
    setStyle documentBody "textAlign" "center"

    Just can <- getCanvas canvas

    -- Use an IORef to communicate between the animation and the event handlers
    state <- newIORef $ GameState currentMatrix currentPalette

    -- Start the animation
    renderMatrix can state

    -- Set an event handler for clicks in the canvas
    canvas `onEvent` Click $ \mouse -> do
        let pos = mouse2pos $  mouseCoords mouse
            index = pos2Index pos
        gamestate <- trace(show pos) trace(show index) readIORef state
        let newmatrix =  updateN (matrix gamestate) index 8
            newpalette = palette gamestate
        writeIORef state $ GameState newmatrix newpalette

    changepalette `onEvent` Click $ \_ -> do
      gamestate <- readIORef state
      let npalette = [(RGB 128 128 128), (RGB 27 32 244), (RGB 188 23 182), (RGB 232 231 12)] 
      writeIORef state (GameState currentMatrix npalette)

    -- Set an event handler for the clear button
    clear `onEvent` Click $ \_ -> writeIORef state $ GameState currentMatrix currentPalette 

-- Note: The current version of Haste does not run the event handler
-- concurrently with the animation, so there's no risk of a race between the
-- different uses of `writeIORef`. If it was truly concurrent, then atomic
-- operations would have to be used instead (see the Data.IORef documentation).

