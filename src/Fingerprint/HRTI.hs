module Fingerprint.HRTI (
    name
  , semantics
  ) where

import Control.Monad.State.Strict

import System.CPUTime

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


toMicro :: (I i) => Integer -> i
toMicro = fromInteger . (`div` ten6)


getPicoTime :: IO Integer
getPicoTime = getCPUTime


gInstr :: (I i) => Instruction i ()
gInstr = do
  t <- liftIO $ do
    a <- getPicoTime
    b <- getPicoTime
    return $ toMicro $ b - a
  pushInstr t

mInstr :: (I i) => Instruction i ()
mInstr = do
  t <- liftIO getPicoTime
  modify $ withIp $ \ip -> ip { hrtiMark = Just t }

tInstr :: (I i) => Instruction i ()
tInstr = do
  ip <- gets currentIp
  case hrtiMark ip of
    Nothing -> reverseInstr
    Just past -> do
      now <- liftIO getPicoTime
      pushInstr $ toMicro $ now - past

eInstr :: (I i) => Instruction i ()
eInstr = modify $ withIp $ \ip -> ip { hrtiMark = Nothing }

sInstr :: (I i) => Instruction i ()
sInstr = do
  pico <- liftIO getPicoTime
  pushInstr $ fromInteger $ pico `div` ten6

