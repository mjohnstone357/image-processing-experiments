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
    -- yPlane' = B.map reduceByte (yPlane frame) -- Make it darker
    -- cbPlane' = B.map reduceByte (cbPlane frame) -- Make yellow
    -- crPlane' = B.map reduceByte (crPlane frame) -- Make light blue

    cbPlane' = B.map nullOut (cbPlane frame)
    crPlane' = B.map nullOut (crPlane frame)
  in
    frame{
--    yPlane = yPlane'
    cbPlane = cbPlane',
    crPlane = crPlane'
    }

nullOut :: W.Word8 -> W.Word8
nullOut = const 128

reduceByte :: W.Word8 -> W.Word8
reduceByte x = x `div` 2
