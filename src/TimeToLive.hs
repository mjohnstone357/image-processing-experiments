module TimeToLive where

import Data.Array

import Parameters
import Detection(DetectionsArray(..))

data TTLArray = TTLArray (Array (Int, Int) Int)

initialTTLArray :: BoxDimensions -> Int -> Int -> TTLArray
initialTTLArray boxDimensions width height =
  let horizontalBoxes = width `div` (boxWidth boxDimensions)
      verticalBoxes = height `div` (boxHeight boxDimensions)
      bounds' = ((0, 0), (horizontalBoxes - 1, verticalBoxes - 1))
      zeroes = repeat 0
      indices' = range bounds'
      values' = zip indices' zeroes
  in
    TTLArray $ array bounds' values'

renewTTLs :: TTLArray -> DetectionsArray -> TTLArray
renewTTLs (TTLArray ttlArray) (DetectionsArray movementsArray) =
  let bounds' = bounds ttlArray
      indices' = indices ttlArray
      ttlValues = elems ttlArray
      movementValues = elems movementsArray
      newElems = zipWith (\ttl movement -> if movement then min (ttl + 8) 32 else ttl `div` 2)
        ttlValues movementValues
  in
    TTLArray $ array bounds' (zip indices' newElems)
