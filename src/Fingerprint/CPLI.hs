module Fingerprint.CPLI (
    name
  , semantics
  ) where

import Control.Monad

import Data.Maybe (isNothing)

import Instruction
import Math

-----------------------------------------------------------

name :: String
name = "CPLI"

semantics :: (I i) => [(Char, Instruction i ())]
semantics = [
    ('A', aInstr)
  , ('D', dInstr)
  , ('M', mInstr)
  , ('S', sInstr)
  , ('O', oInstr)
  , ('V', vInstr)
  ]

complexInstr :: (I i) => (i -> i -> i -> i -> (i, i)) -> Instruction i ()
complexInstr op = do
  d <- popInstr
  c <- popInstr
  b <- popInstr 
  a <- popInstr
  let (e, f) = op a b c d
  pushInstr e
  pushInstr f

aInstr :: (I i) => Instruction i ()
aInstr = complexInstr $ \a b c d -> (a + c, b + d)

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)

dInstr :: (I i) => Instruction i ()
dInstr = complexInstr $ \a' b' c' d' -> let
  [a, b, c, d] = map fromIntegral [a', b', c', d']
  denom = c*c + d*d
  in mapPair fromInteger ((a*c + b*d) `div` denom, (b*c - a*d) `div` denom)

mInstr :: (I i) => Instruction i ()
mInstr = complexInstr $ \a' b' c' d' -> let
  [a, b, c, d] = map fromIntegral [a', b', c', d']
  in mapPair fromInteger (a*c - b*d, b*c + a*d)

sInstr :: (I i) => Instruction i ()
sInstr = complexInstr $ \a b c d -> (a - c, b - d)

oInstr :: (I i) => Instruction i ()
oInstr = do
  b <- popInstr
  a <- popInstr
  let signB = if b >= 0 then "+" else "-"
  outcome <- tryLiftIO $ putStr $ '(' : show a ++ signB ++ show (abs b) ++ "i) "
  when (isNothing outcome) reverseInstr
  

vInstr :: (I i) => Instruction i ()
vInstr = op2Instr $ \a' b' -> let
  (a, b) = mapPair fromIntegral (a', b')
  in fromInteger $ squareRoot $ a*a + b*b

