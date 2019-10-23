module TimeToLive where

import Data.Array

import Parameters
import Movement(MovementsArray(..))

data TTLArray = TTLArray (Array (Int, Int) Int)

initialTTLArray :: Int -> Int -> TTLArray
initialTTLArray width height =
  let horizontalBoxes = width `div` boxWidth
      verticalBoxes = height `div` boxHeight
      bounds' = ((0, 0), (horizontalBoxes - 1, verticalBoxes - 1))
      zeroes = repeat 0
      indices' = range bounds'
      values' = zip indices' zeroes
  in
    TTLArray $ array bounds' values'

renewTTLs :: TTLArray -> MovementsArray -> TTLArray
renewTTLs (TTLArray ttlArray) (MovementsArray movementsArray) =
  let bounds' = bounds ttlArray
      indices' = indices ttlArray
      ttlValues = elems ttlArray
      movementValues = elems movementsArray
      newElems = zipWith (\ttl movement -> if movement then min (ttl + 8) 32 else ttl `div` 2)
        ttlValues movementValues
  in
    TTLArray $ array bounds' (zip indices' newElems)
