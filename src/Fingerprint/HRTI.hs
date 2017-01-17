module Fingerprint.HRTI (
    name
  , semantics
  ) where

import Control.Monad.State.Strict

import Data.Fixed
import Data.Time.Clock
import Data.Time.LocalTime

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


secToMicro :: (I i) => Pico -> i
secToMicro = fromInteger . floor . ((10 ^ (6 :: Integer)) *)


picoToMicro :: (I i) => Integer -> i
picoToMicro = fromInteger . (`div` (10 ^ (6 :: Integer)))


getPicoTime :: IO Integer
getPicoTime = getCPUTime


gInstr :: (I i) => Instruction i ()
gInstr = do
  t <- liftIO $ do
    a <- getPicoTime
    b <- getPicoTime
    return $ picoToMicro $ b - a
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
      pushInstr $ picoToMicro $ now - past

eInstr :: (I i) => Instruction i ()
eInstr = modify $ withIp $ \ip -> ip { hrtiMark = Nothing }

sInstr :: (I i) => Instruction i ()
sInstr = do
  now <- liftIO getCurrentTime
  timeZone <- liftIO getCurrentTimeZone
  let localTime = utcToLocalTime timeZone now
  let (TimeOfDay _ _ sec) = localTimeOfDay localTime
  pushInstr $ secToMicro sec



