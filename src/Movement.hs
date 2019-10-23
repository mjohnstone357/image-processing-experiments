module Movement where

import Data.Array
import qualified Data.ByteString.Lazy as B
import Data.Word(Word8)

import Lib(Frame(..))
import Parameters(BoxDimensions(..))
import Vector(Vector(..), XYVector(..), movementVector, VectorArray(..))

xyMovementBetweenFrames :: BoxDimensions -> Int -> Int -> Frame -> Frame -> Array (Int, Int) XYVector
xyMovementBetweenFrames boxDimensions width height frame1 frame2 =
  let centres1 = centresOfGravity boxDimensions width height frame1
      centres2 = centresOfGravity boxDimensions width height frame2
  in
    movementXYVectors centres1 centres2

movementBetweenFrames :: BoxDimensions -> Int -> Int -> Frame -> Frame -> VectorArray Vector
movementBetweenFrames boxDimensions width height frame1 frame2 =
  let centres1 = centresOfGravity boxDimensions width height frame1
      centres2 = centresOfGravity boxDimensions width height frame2
  in
    movementVectors centres1 centres2

movementVectors :: Array (Int, Int) (Double, Double) -> Array (Int, Int) (Double, Double) -> VectorArray Vector
movementVectors a1 a2 =
  let initialResultArray = movementXYVectors a1 a2
  in
    VectorArray $ fmap movementVector initialResultArray

movementXYVectors :: Array (Int, Int) (Double, Double) -> Array (Int, Int) (Double, Double) -> Array (Int, Int) XYVector
movementXYVectors array1 array2 =
  let indices' = indices array1
      bounds' = bounds array1
      xyVectors = computeXYVectors (elems array1) (elems array2)
      indicesToVectors = zip indices' xyVectors
  in
    array bounds' indicesToVectors

computeXYVectors :: [(Double, Double)] -> [(Double, Double)] -> [XYVector]
computeXYVectors = zipWith xyDiff

xyDiff :: (Double, Double) -> (Double, Double) -> XYVector
xyDiff (x1,y1) (x2,y2) = XYVector {
  xDelta = x2 - x1,
  yDelta = y2 - y1
  }

centresOfGravity :: BoxDimensions -> Int -> Int -> Frame -> Array (Int, Int) (Double, Double)
centresOfGravity boxDimensions width height frame  =
  let arrayWidth = width `div` (boxWidth boxDimensions)
      arrayHeight = height `div` (boxHeight boxDimensions)
      bounds' = ((0,0), (arrayWidth - 1, arrayHeight - 1))
      boxIndices = range bounds'
      centresOfGravity' = (map (\ix -> getCentreOfGravityForBox boxDimensions width frame ix) boxIndices) :: [(Double, Double)]
  in
    array bounds' (zip boxIndices centresOfGravity')

getCentreOfGravityForBox :: BoxDimensions -> Int -> Frame -> (Int, Int) -> (Double, Double)
getCentreOfGravityForBox boxDimensions width frame (x, y) =
  let yPlane' = yPlane frame -- Effectively the greyscale image
      boxWidth' = boxWidth boxDimensions
      boxHeight' = boxHeight boxDimensions
      topLeftPixelOfBox = (boxWidth' * x, boxHeight' * y)
      bottomRightPixelOfBox = ((x + 1) * boxWidth' - 1, (y + 1) *  boxHeight' - 1)
      pixelCoordsForBox = (range (topLeftPixelOfBox, bottomRightPixelOfBox)) :: [(Int, Int)]
      valuesForThosePixels = map (\ix -> yPlane' `B.index` (fromIntegral (flattenIndex width ix))) pixelCoordsForBox
      relativeCoordinates = range ((0, 0), (boxWidth' - 1, boxHeight' - 1))
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
