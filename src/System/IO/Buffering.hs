module System.IO.Buffering (
    withBuffering
  , BufferMode (..)
  ) where

import System.IO

-----------------------------------------------------------

withBuffering :: BufferMode -> Handle -> (Handle -> IO a) -> IO a
withBuffering buff hdl io = do
  origBuff <- hGetBuffering hdl
  hSetBuffering hdl buff
  res <- io hdl
  hSetBuffering hdl origBuff
  return res

