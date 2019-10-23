module Highlighting where

import Data.Array
import Vector

import Lib(Frame(..))
import Detection(movementBetweenFrames)

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

vectorsSignificantlyDifferent :: Vector -> Vector -> Bool
vectorsSignificantlyDifferent v1 v2 =
  -- TODO We need to take the direction into account?
  magnitude v1 > 22 && magnitude v2 < 22
