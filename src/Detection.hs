module Detection where

import Data.Array

import Lib(Frame(..))
import Movement(movementBetweenFrames)
import Vector(XYVector(..), VectorArray(..), subtractVector, vectorMagnitude)
import Parameters(smallBox, largeBox)

data DetectionsArray = DetectionsArray (Array (Int, Int) Bool)

-- Yields an array indicating whether there was movement in each box, compared
-- to the larger box surrounding it
computeDetectionsArray :: Int -> Int -> (Frame, Frame) -> DetectionsArray
computeDetectionsArray width height (frame1, frame2) =
  let fineMovementArray = movementBetweenFrames smallBox width height frame1 frame2
      coarseMovementArray = movementBetweenFrames largeBox width height frame1 frame2
  in
    computeDetections coarseMovementArray fineMovementArray

computeDetections :: VectorArray XYVector -> VectorArray XYVector -> DetectionsArray
computeDetections (VectorArray coarseMovementArray) (VectorArray fineMovementArray) =
  let (_, (maxCX, maxCY)) = bounds coarseMovementArray
      (_, (maxFX, maxFY)) = bounds fineMovementArray
      heightRatio = maxFY `div` maxCY
      widthRatio = maxFX `div` maxCX
      indices' = indices fineMovementArray
      detections = map (\(x, y) -> vectorsDifferent
                         (coarseMovementArray ! (x `div` widthRatio, y `div` heightRatio))
                         (fineMovementArray ! (x, y))) indices'
  in
    DetectionsArray $ array (bounds fineMovementArray) (zip indices' detections)

vectorsDifferent :: XYVector -> XYVector -> Bool
vectorsDifferent v1 v2 =
  let vectorDiff = subtractVector v1 v2
      magnitude' = vectorMagnitude vectorDiff
  in
    magnitude' > 40
