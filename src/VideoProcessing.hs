module VideoProcessing where

import Data.Array
import qualified Data.ByteString.Lazy as B
import qualified Data.Word as W

import Lib(Video(..), Frame(..))
import Detection
import Parameters(boxWidth, boxHeight)
import Vector(XYVector(..), renderFloat)
import Highlighting(HighlightsArray(..), computeHighlights)
import TimeToLive(initialTTLArray, TTLArray)

data DebugFrame = DebugFrame Frame String

transformVideo :: Video Frame -> Video DebugFrame
transformVideo video =
  let oldFrames = (frames video) :: [Frame]
      frameWindows = (slidingWindow oldFrames) :: [(Frame, Frame)]
      width = widthPixels video
      height = heightPixels video
      initialTTLArray' = initialTTLArray width height
      debugFrames = getAnnotatedFrames width height initialTTLArray' frameWindows
  in
    video {frames = debugFrames}

getAnnotatedFrames :: Int -> Int -> TTLArray -> [(Frame, Frame)] -> [DebugFrame]
getAnnotatedFrames _ _ _ [] = []
getAnnotatedFrames width height ttlArray ((frame1, frame2):pairs) =
  let (highlightsArray, newTTLArray) = computeHighlights ttlArray width height (frame1, frame2)
      
      newFrame = transformFrame width highlightsArray frame2

      movementArray = (xyMovementBetweenFrames width height frame1 frame2) :: Array (Int, Int) XYVector
      csvVectors = map (\v -> renderFloat (xDelta v) ++ "," ++ renderFloat (yDelta v)) (elems movementArray)
      
      message = "vectors for frame:\n" ++ (unlines csvVectors)
      
      debugFrame = DebugFrame newFrame message
  in
    debugFrame : (getAnnotatedFrames width height newTTLArray pairs)

slidingWindow :: [a] -> [(a, a)]
slidingWindow (x:y:xs) = (x, y) : slidingWindow (y:xs)
slidingWindow _ = []

-- Takes an array indicating whether each box should be highlighted
transformFrame :: Int -> HighlightsArray -> Frame -> Frame
transformFrame width highlightsArray frame =
  let
    pixelFlatIndices = [0..(length unpackedPixels)]
    unpackedPixels = B.unpack (cbPlane frame)
    newPixels = zipWith (transformPixel width highlightsArray) pixelFlatIndices unpackedPixels
    newPackedPixels = B.pack newPixels
    cbPlane' = newPackedPixels
  in
    frame{
    cbPlane = cbPlane'
    }

transformPixel :: Int -> HighlightsArray -> Int -> W.Word8 -> W.Word8
transformPixel width (HighlightsArray highlightsArray) pixelFlatIndex word =
  let effectiveIndexInFullImage = pixelFlatIndex * 2
      (pixelX, pixelY) = (effectiveIndexInFullImage `mod` width, effectiveIndexInFullImage `div` width)
      indexInHighlightsArray = (pixelX `div` boxWidth, 2 * pixelY `div` boxHeight)
  in
    if highlightsArray ! indexInHighlightsArray
    then
      reduceByte word
    else
      word

expandIndex :: (Int, Int) -> (Int, Int)
expandIndex (x, y) = (x * 2, y)

nullOut :: W.Word8 -> W.Word8
nullOut = const 128

reduceByte :: W.Word8 -> W.Word8
reduceByte x = x `div` 2
