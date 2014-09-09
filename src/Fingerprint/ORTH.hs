module Fingerprint.ORTH (
    name
  , semantics
  ) where

import Control.Monad.State.Strict

import Data.Bits
import Data.Maybe (fromMaybe)
import Data.Vector

import Space.Cell
import Space.Space

import Env
import Instruction
import Ip

-----------------------------------------------------------

name :: String
name = "ORTH"

semantics :: (I i) => [(Char, Instruction i ())]
semantics = [
    ('A', aInstr)
  , ('E', eInstr)
  , ('G', gInstr)
  , ('O', oInstr)
  , ('P', pInstr)
  , ('S', sInstr)
  , ('V', vInstr)
  , ('W', wInstr)
  , ('X', xInstr)
  , ('Y', yInstr)
  , ('Z', zInstr)
  ]

aInstr :: (I i) => Instruction i ()
aInstr = op2Instr (.&.)

oInstr :: (I i) => Instruction i ()
oInstr = op2Instr (.|.)

eInstr :: (I i) => Instruction i ()
eInstr = op2Instr xor

overlay :: [Maybe a] -> Vector a -> Vector a
overlay = zipWithV f . mkVector . (++ repeat Nothing)
  where
    f = flip fromMaybe

xInstr :: (I i) => Instruction i ()
xInstr = do
  x <- popInstr
  modify $ withIp $ \ip -> ip { getPos = overlay [Just x] $ getPos ip }

yInstr :: (I i) => Instruction i ()
yInstr = do
  x <- popInstr
  modify $ withIp $ \ip -> ip { getPos = overlay [Nothing, Just x] $ getPos ip }

vInstr :: (I i) => Instruction i ()
vInstr = do
  x <- popInstr
  modify $ withIp $ \ip -> ip { getDelta = overlay [Just x] $ getDelta ip }

wInstr :: (I i) => Instruction i ()
wInstr = do
  x <- popInstr
  modify $ withIp $ \ip -> ip { getDelta = overlay [Nothing, Just x] $ getDelta ip }

gInstr :: (I i) => Instruction i ()
gInstr = guardDim 2 $ do
  loc <- liftM reverseV $ popVectorInstr (2 :: Int)
  env <- get
  let dim = getDim env
      s = getSpace env
      x = ordCell $ cellAt s $ takeV dim $ loc `append` 0
  pushInstr x

pInstr :: (I i) => Instruction i ()
pInstr = guardDim 2 $ do
  dim <- gets getDim
  loc <- liftM reverseV $ popVectorInstr (2 :: Int)
  x <- popInstr
  modify $ withSpace $ \s -> putCell s (chrCell x) $ takeV dim $ loc `append` 0

zInstr :: (I i) => Instruction i ()
zInstr = ifInstr nopInstr trampolineInstr

sInstr :: (I i) => Instruction i ()
sInstr = do
  s <- liftM (map $ fromMaybe '?') popStringInstr
  liftIO $ putStr s

