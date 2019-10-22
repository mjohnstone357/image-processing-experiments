module Main where

import qualified Data.ByteString.Lazy as B
import System.IO

import Lib(readVideo, showFrame, Video(..))
import VideoProcessing(transformVideo, DebugFrame(..))

main :: IO ()
main = do
  input <- B.getContents
  let inputVideo = readVideo input
      debuggableVideo = transformVideo inputVideo
  outputDebugVideo debuggableVideo

outputDebugVideo :: Video DebugFrame -> IO ()
outputDebugVideo video = do
  let debugFrames = frames video
  B.putStr $ headerMagic video
  withFile "/home/matt/extraction/debug.log" WriteMode $
    setModeAndOutput debugFrames

setModeAndOutput :: [DebugFrame] -> Handle -> IO ()
setModeAndOutput fs handle = do
  hSetBuffering handle LineBuffering
  outputFrames fs handle

outputFrames :: [DebugFrame] -> Handle -> IO ()
outputFrames fs handle = mapM_ (outputFrame handle) fs

outputFrame :: Handle -> DebugFrame -> IO ()
outputFrame handle frame = do
  let (DebugFrame videoFrame message) = frame
      frameBytes = showFrame videoFrame
  B.putStr frameBytes
  hPutStrLn handle message
