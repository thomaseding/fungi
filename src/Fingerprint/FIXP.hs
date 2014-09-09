module Fingerprint.FIXP (
    name
  , semantics
  ) where

import Prelude hiding (pi)
import qualified Prelude

import Control.Monad.State.Strict

import Data.Bits ((.&.), (.|.), xor)

import Instruction
import Math
import Random

-----------------------------------------------------------

name :: String
name = "FIXP"

semantics :: (I i) => [(Char, Instruction i ())]
semantics = [
    ('A', aInstr)
  , ('B', bInstr)
  , ('C', cInstr)
  , ('D', dInstr)
  , ('I', iInstr)
  , ('J', jInstr)
  , ('N', nInstr)
  , ('O', oInstr)
  , ('P', pInstr)
  , ('Q', qInstr)
  , ('R', rInstr)
  , ('S', sInstr)
  , ('T', tInstr)
  , ('U', uInstr)
  , ('V', vInstr)
  , ('X', xInstr)
  ]

trigInstr :: (I i) => (Double -> Double) -> Instruction i ()
trigInstr op = opInstr $ \deg -> let
  deg' = fromInteger $ fromIntegral deg `mod` (360 * scale)
  rad = pi * deg' / scale / 180
  res = op rad
  in round $ res * scale
  where
    pi = Prelude.pi
    scale :: (Num a) => a
    scale = 10000

itrigInstr :: (I i) => (Double -> Double) -> Instruction i ()
itrigInstr op = opInstr $ \x -> let
  x' = fromIntegral x / scale
  rad = op x'
  deg = rad * 180 / pi
  in round $ deg * scale
  where
    pi = Prelude.pi
    scale :: (Num a) => a
    scale = 10000

aInstr :: (I i) => Instruction i ()
aInstr = op2Instr (.&.)

bInstr :: (I i) => Instruction i ()
bInstr = itrigInstr acos

cInstr :: (I i) => Instruction i ()
cInstr = trigInstr cos

dInstr :: (I i) => Instruction i ()
dInstr = popInstr >>= liftIO . uniformN >>= pushInstr
  where
    uniformN n = liftM (signum n *) $ uniformR (0, abs n)

iInstr :: (I i) => Instruction i ()
iInstr = trigInstr sin

jInstr :: (I i) => Instruction i ()
jInstr = itrigInstr asin

nInstr :: (I i) => Instruction i ()
nInstr = opInstr negate

oInstr :: (I i) => Instruction i ()
oInstr = op2Instr (.|.)

pInstr :: (I i) => Instruction i ()
pInstr = opInstr $ \x -> fromInteger $ (fromIntegral x * pi) `div` pow10
  where
    pi = 3141592653589793
    pow10 = 10 ^ (length (show pi) - 1)

qInstr :: (I i) => Instruction i ()
qInstr = opInstr $ \x -> if x >= 0
  then fromInteger $ squareRoot $ fromIntegral x
  else x

rInstr :: (I i) => Instruction i ()
rInstr = op2Instr $ \x y -> if y >= 0
  then x ^ y
  else 0

sInstr :: (I i) => Instruction i ()
sInstr = opInstr signum

tInstr :: (I i) => Instruction i ()
tInstr = trigInstr tan

uInstr :: (I i) => Instruction i ()
uInstr = itrigInstr atan

vInstr :: (I i) => Instruction i ()
vInstr = opInstr abs

xInstr :: (I i) => Instruction i ()
xInstr = op2Instr xor

