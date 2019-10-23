module Highlighting where

import Data.Array

import Lib(Frame(..))
import TimeToLive(TTLArray(..), renewTTLs)
import Movement(computeMovementArray)

data HighlightsArray = HighlightsArray (Array (Int, Int) Bool)

computeHighlights :: TTLArray -> Int -> Int -> (Frame, Frame) -> (HighlightsArray, TTLArray)
computeHighlights ttlArray width height framePair  =
  let currentMovementArray = computeMovementArray width height framePair
      newTTLArray = renewTTLs ttlArray currentMovementArray
      highlightsArray = resolveTTLs newTTLArray
  in
    (highlightsArray, newTTLArray)

resolveTTLs :: TTLArray -> HighlightsArray
resolveTTLs (TTLArray arr) = HighlightsArray $ fmap (\x -> x > 0) arr
