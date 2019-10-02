module Main where

import qualified Data.ByteString.Lazy as B

import Lib

main :: IO ()
main = do
  B.interact processVideo
