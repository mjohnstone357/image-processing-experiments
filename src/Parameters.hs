module Parameters where

data BoxDimensions = BoxDimensions {
  boxWidth :: Int,
  boxHeight :: Int
  }


smallBox :: BoxDimensions
smallBox = BoxDimensions {
  boxWidth = 8,
  boxHeight = 8
  }

largeBox :: BoxDimensions
largeBox = BoxDimensions {
  boxWidth = 16,
  boxHeight = 16
  }
