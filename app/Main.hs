module Main where

import qualified Data.ByteString.Lazy as B
import Control.Concurrent

import Lib(readVideo, showVideo)
import VideoProcessing(transformVideo, DebuggableVideo(..))

main :: IO ()
main = do
  input <- B.getContents
  let inputVideo = readVideo input
      transformedVideo = transformVideo inputVideo
      outputStream = showVideo (vid transformedVideo)
      outputString = unlines (debugStream transformedVideo)
  B.putStr outputStream
  writeFile "/home/matt/extraction/debug.log" outputString

-- TODO Use one thread to output the debug info and video stream
