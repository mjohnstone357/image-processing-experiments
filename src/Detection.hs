module Detection where

import Data.Array

import Lib(Frame(..))
import Movement(movementBetweenFrames)
import Vector(Vector(..), VectorArray(..))
import Parameters(BoxDimensions)

data DetectionsArray = DetectionsArray (Array (Int, Int) Bool)

-- Yields an array indicating whether there was movement in each box, compared
-- to the larger box surrounding it
computeDetectionsArray :: BoxDimensions -> Int -> Int -> (Frame, Frame) -> DetectionsArray
computeDetectionsArray boxDimensions width height (frame1, frame2) =
  let (VectorArray movementArray) = movementBetweenFrames boxDimensions width height frame1 frame2
      indices' = indices movementArray
      bounds' = bounds movementArray
      detections = map (movementDifferentToNeighbours (VectorArray movementArray)) indices'
      detectionsArray = DetectionsArray $ array bounds' (zip indices' detections)
  in
    detectionsArray

movementDifferentToNeighbours :: VectorArray Vector -> (Int, Int) -> Bool
movementDifferentToNeighbours (VectorArray arr) (x, y) =
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

vectorsSignificantlyDifferent :: Vector -> Vector -> Bool
vectorsSignificantlyDifferent v1 v2 =
  -- TODO We need to take the direction into account?
  magnitude v1 > 22 && magnitude v2 < 22
