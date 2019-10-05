module VideoProcessing where

import Data.Array
import qualified Data.ByteString.Lazy as B
import qualified Data.Word as W

import Lib(showVideo, readVideo, Video(..), Frame(..))
import Detection
import Parameters(boxWidth, boxHeight)

processVideo :: B.ByteString -> B.ByteString
processVideo = showVideo . transformVideo . readVideo

transformVideo :: Video -> Video
transformVideo video =
  let oldFrames = (frames video) :: [Frame]
      frameWindows = (slidingWindow oldFrames) :: [(Frame, Frame)]
      width = widthPixels video
      height = heightPixels video
      annotatedFrames = map (annotatedFrame width height) frameWindows
  in
    video{frames = annotatedFrames}

annotatedFrame :: Int -> Int -> (Frame, Frame) -> Frame
annotatedFrame width height (frame1, frame2) =
  let movementArray = movementBetweenFrames width height frame1 frame2
      indices' = indices movementArray
      bounds' = bounds movementArray
      highlights = map (movementDifferentToNeighbours movementArray) indices'
      highlightsArray = array bounds' (zip indices' highlights)
--      highlightsArray = array bounds' [((x,y), x > 10 && x < 40 && y > 35 && y < 70) | (x,y) <- indices']
  in
    transformFrame width height highlightsArray frame2

movementDifferentToNeighbours :: Array (Int, Int) Vector -> (Int, Int) -> Bool
movementDifferentToNeighbours arr (x, y) =
  let (_, (maxX, maxY)) = bounds arr
      thisVector = arr ! (x, y)
      neighbourIndices = concat [
        if x > 0 then [(x - 1, y)] else [],
        if y > 0 then [(x, y - 1)] else [],
        if x < maxX then [(x + 1, y)] else [],
        if y < maxY then [(x, y + 1)] else []
        ]
      neighbourVectors = (map (\ix -> arr ! ix) neighbourIndices) :: [Vector]
  in
--    any (vectorsSignificantlyDifferent thisVector) neighbourVectors
    magnitude thisVector > 5 -- && direction thisVector > 0.6 && direction thisVector < 1.2

vectorsSignificantlyDifferent :: Vector -> Vector -> Bool
vectorsSignificantlyDifferent v1 v2 =
  let minMagnitude = 2
  in
    magnitude v1 > minMagnitude && magnitude v2 > minMagnitude &&
    abs (direction v1 - direction v2) > (pi / 2)

slidingWindow :: [a] -> [(a, a)]
slidingWindow (x:y:xs) = (x, y) : slidingWindow (y:xs)
slidingWindow _ = []

-- Takes an array indicating whether each box should be highlighted
transformFrame :: Int -> Int -> Array (Int, Int) Bool -> Frame -> Frame
transformFrame width height highlightsArray frame =
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

-- transformPixel :: Int -> Array (Int, Int) Bool -> Int -> W.Word8 -> W.Word8
-- transformPixel width highlightsArray pixelFlatIndex word =
--   let effectiveWidth = width `div` 2
--   in
--     if pixelFlatIndex < effectiveWidth
--     then
--       reduceByte word
--     else
--       word

transformPixel :: Int -> Array (Int, Int) Bool -> Int -> W.Word8 -> W.Word8
transformPixel width highlightsArray pixelFlatIndex word =
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
