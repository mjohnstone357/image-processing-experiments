module Detection where

import Data.Array
import qualified Data.ByteString.Lazy as B
import Data.Word(Word8)

import Lib(Frame(..))
import Parameters(boxWidth, boxHeight)

data Vector = Vector {
  magnitude :: Double,
  direction :: Double
  }

movementBetweenFrames :: Int -> Int -> Frame -> Frame -> Array (Int, Int) Vector
movementBetweenFrames width height frame1 frame2 =
  let centres1 = centresOfGravity width height frame1
      centres2 = centresOfGravity width height frame2
  in
    movementVectors centres1 centres2

movementVectors :: Array (Int, Int) (Double, Double) -> Array (Int, Int) (Double, Double) -> Array (Int, Int) Vector
movementVectors array1 array2 =
  let indices' = indices array1
      bounds' = bounds array1
      vectors = zipWith movementVector (elems array1) (elems array2)
      indicesToVectors = zip indices' vectors
  in
    array bounds' indicesToVectors

movementVector :: (Double, Double) -> (Double, Double) -> Vector
movementVector (x, y) (p, q) =
  let deltaX = p - x
      deltaY = q - y
      angleInRadians = atan (deltaY / deltaX)
      magnitude' = sqrt (deltaX ^ 2 + deltaY ^ 2)
  in
    Vector {
    magnitude = magnitude',
    direction = angleInRadians
    }

centresOfGravity :: Int -> Int -> Frame -> Array (Int, Int) (Double, Double)
centresOfGravity width height frame  =
  let arrayWidth = width `div` boxWidth
      arrayHeight = height `div` boxHeight
      bounds' = ((0,0), (arrayWidth - 1, arrayHeight - 1))
      boxIndices = range bounds'
      centresOfGravity = (map (\ix -> getCentreOfGravityForBox width frame ix) boxIndices) :: [(Double, Double)]
  in
    array bounds' (zip boxIndices centresOfGravity)

getCentreOfGravityForBox :: Int -> Frame -> (Int, Int) -> (Double, Double)
getCentreOfGravityForBox width frame (x, y) =
  let yPlane' = yPlane frame -- Effectively the greyscale image
      topLeftPixelOfBox = (boxWidth * x, boxHeight * y)
      bottomRightPixelOfBox = ((x + 1) * boxWidth - 1, (y + 1) *  boxHeight - 1)
      pixelCoordsForBox = (range (topLeftPixelOfBox, bottomRightPixelOfBox)) :: [(Int, Int)]
      valuesForThosePixels = map (\ix -> yPlane' `B.index` (fromIntegral (flattenIndex width ix))) pixelCoordsForBox
      relativeCoordinates = range ((0, 0), (boxWidth - 1, boxHeight - 1))
      weightedCoordinates = zipWith weightedCoordinate relativeCoordinates valuesForThosePixels
      weightedAverageCoordinate = averageWeightedCoordinates weightedCoordinates
  in
--    error $ show bottomRightPixelOfBox
    weightedAverageCoordinate

squashIndex :: (Int, Int) -> (Int, Int)
squashIndex (x, y) = (x `div` 2, y)

averageWeightedCoordinates :: [(Double, Double)] -> (Double, Double)
averageWeightedCoordinates coords =
  let numberOfElements = length coords
      totalX = sum $ map fst coords
      totalY = sum $ map snd coords
  in
    (totalX / fromIntegral numberOfElements, totalY / fromIntegral numberOfElements)

weightedCoordinate :: (Int, Int) -> Word8 -> (Double, Double)
weightedCoordinate (x, y) word = (fromIntegral (x * fromIntegral word), fromIntegral (y * fromIntegral word))

flattenIndex :: Int -> (Int, Int) -> Int
flattenIndex width (x, y) = y * width + x
