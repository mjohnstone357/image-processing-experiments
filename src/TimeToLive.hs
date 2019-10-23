module TimeToLive where

import Data.Array

import Parameters

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

resolveTTLs :: Array (Int, Int) Int -> Array (Int, Int) Bool
resolveTTLs = fmap (\x -> x > 0)

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
