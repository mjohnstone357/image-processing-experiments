module Movement where

import Data.Array

import Lib(Frame(..))
import Detection(movementBetweenFrames)
import Vector(Vector(..))

data MovementsArray = MovementsArray (Array (Int, Int) Bool)

-- Yields an array indicating whether there was movement in each box
computeMovementArray :: Int -> Int -> (Frame, Frame) -> MovementsArray
computeMovementArray width height (frame1, frame2) =
  let movementArray = movementBetweenFrames width height frame1 frame2
      indices' = indices movementArray
      bounds' = bounds movementArray
      movements = map (movementDifferentToNeighbours movementArray) indices'
      movementsArray = MovementsArray $ array bounds' (zip indices' movements)
  in
    movementsArray

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
