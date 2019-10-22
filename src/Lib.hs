module Lib
    ( showVideo,
      readVideo,
      showFrame,
      Video(..),
      Frame(..)
    ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

data Video f = Video {
  widthPixels :: Int,
  heightPixels :: Int,
  colourSpace :: ColourSpace,
  headerMagic :: B.ByteString,
  frames :: [f]
  } deriving (Show)

data Frame = Frame {
  yPlane :: B.ByteString,
  cbPlane :: B.ByteString,
  crPlane :: B.ByteString
  } deriving (Show)

splitToFrame :: Int -> Int -> B.ByteString -> Frame
splitToFrame width height bytes =
  let numberOfPixels = fromIntegral $ width * height
      (yPlane', remaining) = B.splitAt numberOfPixels bytes
      (cbPlane', crPlane') = B.splitAt (numberOfPixels `div` 4) remaining
  in
--    error $ "plane length is " ++ (show (B.length crPlane'))
    Frame {
    yPlane = yPlane',
    cbPlane = cbPlane',
    crPlane = crPlane'
    }

extractHeader :: B.ByteString -> [B.ByteString] -> B.ByteString
extractHeader prefix headers =
  head $ filter (\header -> prefix `B.isPrefixOf` header) headers

readVideo :: B.ByteString -> Video Frame
readVideo inputBytes =
  let (headers, remainingInput) = getHeaderWords [] inputBytes
      header = B.intercalate (C.pack " ") headers
      wPixels = read $ tail $ C.unpack $ extractHeader (C.pack "W") headers
      hPixels = read $ tail $ C.unpack $ extractHeader (C.pack "H") headers
      cSpace = readColourSpace $ B.take 3 $ B.tail $ extractHeader (C.pack "C") headers
      frameLength = computeFrameLength cSpace wPixels hPixels
  in
    Video {
    widthPixels = wPixels,
    heightPixels = hPixels,
    colourSpace = cSpace,
    headerMagic = B.concat [header, C.pack "\n"],
    frames = readAllFrames wPixels hPixels frameLength remainingInput
    }

readAllFrames :: Int -> Int -> Int -> B.ByteString -> [Frame]
readAllFrames width height frameLength inputBytes =
  if frameMarker `B.isPrefixOf` inputBytes
  then
    let (currentFrame, remainingBytes) = readOneFrame width height frameLength inputBytes
    in currentFrame : readAllFrames width height frameLength remainingBytes
  else
    error $ "Did not find frame marker, instead found: " ++ (show (B.take 100 inputBytes))

readOneFrame :: Int -> Int -> Int ->  B.ByteString -> (Frame, B.ByteString)
readOneFrame width height frameLength inputBytes =
  let (Just prefixDropped) = B.stripPrefix frameMarker inputBytes
      (currentFrameBytes, remainingFramesBytes) = B.splitAt (fromIntegral frameLength) prefixDropped
  in
    (splitToFrame width height currentFrameBytes, remainingFramesBytes)

showVideo :: Video Frame -> B.ByteString
showVideo video = B.concat $ [headerMagic video] ++ (map showFrame (frames video))

showFrame :: Frame -> B.ByteString
showFrame frame = B.concat [
  initialFrameMarker,
  B.pack [fromIntegral (0x0A :: Int)],
  yPlane frame,
  cbPlane frame,
  crPlane frame
  ]

getHeaderWords :: [B.ByteString] -> B.ByteString -> ([B.ByteString], B.ByteString)
getHeaderWords existingWords inputBytes =
  let (currentWord, everythingElse) = B.span
        (\byte -> byte /= (fromIntegral (0x20 :: Int)) && byte /= (fromIntegral (0x0A :: Int))) inputBytes
      restOfInput = B.tail everythingElse
  in
    if currentWord == initialFrameMarker
    then
      (existingWords, B.concat [frameMarker, restOfInput])
    else
      getHeaderWords (existingWords ++ [currentWord]) restOfInput

initialFrameMarker :: B.ByteString
initialFrameMarker = C.pack "FRAME"

frameMarker :: B.ByteString
frameMarker = B.concat [initialFrameMarker, (B.pack [fromIntegral (0x0A :: Int)])]

data ColourSpace = FourTwoZero | FourTwoTwo | FourFourFour
                 deriving (Show)

readColourSpace :: B.ByteString -> ColourSpace
readColourSpace bytes
  | bytes == C.pack "420" = FourTwoZero
  | bytes == C.pack "422" = FourTwoTwo
  | bytes == C.pack "444" = FourFourFour
  | otherwise = error $ "unknown colour space: " ++ show bytes

computeFrameLength :: ColourSpace -> Int -> Int -> Int
computeFrameLength FourTwoZero width height = (width * height * 3) `div` 2
computeFrameLength FourTwoTwo width height = width * height * 2
computeFrameLength FourFourFour width height = width * height * 3
