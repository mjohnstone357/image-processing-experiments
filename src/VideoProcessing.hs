module VideoProcessing where

import Data.Array
import qualified Data.ByteString.Lazy as B
import qualified Data.Word as W

import Lib(showVideo, readVideo, Video(..), Frame(..))
import Detection
import Parameters(boxWidth, boxHeight)

data DebuggableVideo = DebuggableVideo {
  vid :: Video,
  debugStream :: [String]
  }

transformVideo :: Video -> DebuggableVideo
transformVideo video =
  let oldFrames = (frames video) :: [Frame]
      frameWindows = (slidingWindow oldFrames) :: [(Frame, Frame)]
      width = widthPixels video
      height = heightPixels video
      initialTTLArray' = initialTTLArray width height
      framesWithDebugging = getAnnotatedFrames width height initialTTLArray' frameWindows
      annotatedFrames = map fst framesWithDebugging
  in
    DebuggableVideo {
    vid = video{frames = annotatedFrames},
    debugStream = map snd framesWithDebugging
    }

initialTTLArray :: Int -> Int -> Array (Int, Int) Int
initialTTLArray width height =
  let horizontalBoxes = width `div` boxWidth
      verticalBoxes = height `div` boxHeight
      bounds' = ((0, 0), (horizontalBoxes - 1, verticalBoxes - 1))
      zeroes = repeat 0
      indices' = range bounds'
      values' = zip indices' zeroes
  in
    array bounds' values'

getAnnotatedFrames :: Int -> Int -> Array (Int, Int) Int -> [(Frame, Frame)] -> [(Frame, String)]
getAnnotatedFrames _ _ _ [] = []
getAnnotatedFrames width height ttlArray ((frame1, frame2):pairs) =
  let currentMovementArray = computeMovementArray width height (frame1, frame2)
      newTTLArray = renewTTLs ttlArray currentMovementArray
      highlightsArray = resolveTTLs newTTLArray
      newFrame = transformFrame width height highlightsArray frame2
  in
    (newFrame, "foo") : (getAnnotatedFrames width height newTTLArray pairs)

resolveTTLs :: Array (Int, Int) Int -> Array (Int, Int) Bool
resolveTTLs = fmap (\x -> x > 0)

-- Yields an array indicating whether there was movement in each box
computeMovementArray :: Int -> Int -> (Frame, Frame) -> Array (Int, Int) Bool
computeMovementArray width height (frame1, frame2) =
  let movementArray = movementBetweenFrames width height frame1 frame2
      indices' = indices movementArray
      bounds' = bounds movementArray
      highlights = map (movementDifferentToNeighbours movementArray) indices'
      highlightsArray = (array bounds' (zip indices' highlights)) :: Array (Int, Int) Bool
  in
    highlightsArray
  
renewTTLs :: Array (Int, Int) Int -> Array (Int, Int) Bool -> Array (Int, Int) Int
renewTTLs ttlArray movementsArray =
  let bounds' = bounds ttlArray
      indices' = indices ttlArray
      ttlValues = elems ttlArray
      movementValues = elems movementsArray
      newElems = zipWith (\ttl movement -> if movement then min (ttl + 8) 32 else ttl `div` 2)
        ttlValues movementValues
  in
    array bounds' (zip indices' newElems)

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
    any (vectorsSignificantlyDifferent thisVector) neighbourVectors
--    magnitude thisVector > 5 -- && direction thisVector > 0.6 && direction thisVector < 1.2

-- TODO somehow "subtract" the prevailing movement from the vectors
subtractVector :: Vector -> Vector -> Vector
subtractVector = undefined

vectorsSignificantlyDifferent :: Vector -> Vector -> Bool
vectorsSignificantlyDifferent v1 v2 =
  -- TODO We need to take the direction into account?
  magnitude v1 > 22 && magnitude v2 < 22

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
