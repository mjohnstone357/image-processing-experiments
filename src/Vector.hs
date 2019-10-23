module Vector where

import Data.Array
import Numeric

data VectorArray v = VectorArray (Array (Int, Int) v)

data Vector = Vector {
  magnitude :: Double,
  direction :: Double
  } deriving (Show)

data XYVector = XYVector {
  xDelta :: Double,
  yDelta :: Double
  } deriving (Show)

renderFloat :: Double -> String
renderFloat num = showFFloat Nothing num ""

humanReadable :: Vector -> String
humanReadable vector =
  let renderedDirection = renderDirection $ (direction vector)
      renderedMagnitude = (round (magnitude vector * 10)) :: Int
  in
    if renderedMagnitude < 10
    then
      "Steady"
    else
      renderedDirection ++ " - " ++ show renderedMagnitude

renderDirection :: Double -> String
renderDirection theta
  | theta > (-pi / 2) && theta < (pi / 2) = "Right"
  | otherwise = "Left"

averageXYVector :: [XYVector] -> XYVector
averageXYVector vectors =
  let xDeltas = map xDelta vectors
      yDeltas = map yDelta vectors
      size = fromIntegral $ length xDeltas
  in
    XYVector {
    xDelta = sum xDeltas / size,
    yDelta = sum yDeltas / size
    }

movementVector :: XYVector -> Vector
movementVector xyVector =
  let deltaX' = xDelta xyVector
      deltaY' = yDelta xyVector
      angleInRadians = atan2 deltaY' deltaX'
      theNumberTwo = 2 :: Int
      magnitude' = sqrt (deltaX' ^ theNumberTwo + deltaY' ^ theNumberTwo)
  in
    Vector {
    magnitude = magnitude',
    direction = angleInRadians
    }
