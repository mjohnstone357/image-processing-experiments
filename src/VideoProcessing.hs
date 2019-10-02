module VideoProcessing where

import qualified Data.ByteString.Lazy as B
import qualified Data.Word as W

import Lib(showVideo, readVideo, Video(..), Frame(..))

processVideo :: B.ByteString -> B.ByteString
processVideo = showVideo . transformVideo . readVideo

transformVideo :: Video -> Video
transformVideo video =
  let oldFrames = frames video
      newFrames = map transformFrame oldFrames
  in
    video{frames = newFrames}

transformFrame :: Frame -> Frame
transformFrame frame =
  let 
    yPlane' = B.map reduceByte (yPlane frame) -- Make it darker
    cbPlane' = B.map reduceByte (cbPlane frame) -- Make it ???
    crPlane' = B.map reduceByte (crPlane frame) -- Make it ???
      
  in
    frame{
--    yPlane = yPlane'
    cbPlane = cbPlane'
--    crPlane = cbPlane'
    }

reduceByte :: W.Word8 -> W.Word8
reduceByte x = x `div` 2
