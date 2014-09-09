module Fingerprint.HRTI (
    name
  , semantics
  ) where

import Control.Monad.State.Strict

import System.Time

import Env
import Instruction
import Ip

-----------------------------------------------------------

name :: String
name = "HRTI"

semantics :: (I i) => [(Char, Instruction i ())]
semantics = [
    ('G', gInstr)
  , ('M', mInstr)
  , ('T', tInstr)
  , ('E', eInstr)
  , ('S', sInstr)
  ]

ten6 :: Integer
ten6 = 10 ^ (6 :: Integer)

diffMicro :: (I i) => ClockTime -> ClockTime -> i
diffMicro x = fromInteger . (`div` ten6) . tdPicosec . diffClockTimes x

gInstr :: (I i) => Instruction i ()
gInstr = do
  t <- liftIO $ do
    _ <- getClockTime
    x <- getClockTime
    y <- getClockTime
    return $ diffMicro y x
  pushInstr t

mInstr :: (I i) => Instruction i ()
mInstr = do
  t <- liftIO getClockTime
  modify $ withIp $ \ip -> ip { hrtiMark = Just t }

tInstr :: (I i) => Instruction i ()
tInstr = do
  ip <- gets currentIp
  case hrtiMark ip of
    Nothing -> reverseInstr
    Just t -> do
      t' <- liftIO getClockTime
      pushInstr $ diffMicro t' t

eInstr :: (I i) => Instruction i ()
eInstr = modify $ withIp $ \ip -> ip { hrtiMark = Nothing }

sInstr :: (I i) => Instruction i ()
sInstr = do
  pico <- liftIO $ liftM ctPicosec $ getClockTime >>= toCalendarTime
  pushInstr $ fromInteger $ pico `div` ten6

