module Lib
    ( processVideo
    ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

data Video = Video {
  widthPixels :: Int,
  heightPixels :: Int,
  colourSpace :: ColourSpace,
  headerMagic :: B.ByteString,
  frames :: [Frame]
  } deriving (Show)

data Frame = Frame B.ByteString
           deriving (Show)

processVideo :: B.ByteString -> B.ByteString
processVideo = showVideo . transformVideo . readVideo

extractHeader :: B.ByteString -> [B.ByteString] -> B.ByteString
extractHeader prefix headers =
  head $ filter (\header -> prefix `B.isPrefixOf` header) headers

readVideo :: B.ByteString -> Video
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
    frames = readAllFrames frameLength remainingInput
    }

readAllFrames :: Int -> B.ByteString -> [Frame]
readAllFrames frameLength inputBytes =
  if frameMarker `B.isPrefixOf` inputBytes
  then
    let (currentFrame, remainingBytes) = readOneFrame frameLength inputBytes
    in currentFrame : readAllFrames frameLength remainingBytes
  else
    error $ "Did not find frame marker, instead found: " ++ (show (B.take 100 inputBytes))

readOneFrame ::Int ->  B.ByteString -> (Frame, B.ByteString)
readOneFrame frameLength inputBytes =
  let (Just prefixDropped) = B.stripPrefix frameMarker inputBytes
      (currentFrameBytes, remainingFramesBytes) = B.splitAt (fromIntegral frameLength) prefixDropped
  in
    (Frame currentFrameBytes, remainingFramesBytes)

showVideo :: Video -> B.ByteString
showVideo video = B.concat $ [headerMagic video] ++ (map showFrame (frames video))

showFrame :: Frame -> B.ByteString
showFrame (Frame frameBytes) = B.concat [initialFrameMarker, B.pack [fromIntegral (0x0A :: Int)], frameBytes]

transformVideo :: Video -> Video
transformVideo = id

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
