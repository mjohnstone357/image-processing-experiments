module Highlighting where

import Data.Array

import Lib(Frame(..))
import TimeToLive(TTLArray(..), renewTTLs)
import Detection(computeDetectionsArray)

data HighlightsArray = HighlightsArray (Array (Int, Int) Bool)

computeHighlights :: TTLArray -> Int -> Int -> (Frame, Frame) -> (HighlightsArray, TTLArray)
computeHighlights ttlArray width height framePair =
  let currentDetectionsArray = computeDetectionsArray width height framePair
      newTTLArray = renewTTLs ttlArray currentDetectionsArray
      highlightsArray = resolveTTLs newTTLArray
  in
    (highlightsArray, newTTLArray)

resolveTTLs :: TTLArray -> HighlightsArray
resolveTTLs (TTLArray arr) = HighlightsArray $ fmap (\x -> x > 0) arr
