module Main where

import qualified Data.ByteString.Lazy as B

import VideoProcessing(processVideo)

main :: IO ()
main = do
  B.interact processVideo
