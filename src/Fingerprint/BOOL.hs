module Fingerprint.BOOL (
    name
  , semantics
  ) where

import Instruction

-----------------------------------------------------------

name :: String
name = "BOOL"

semantics :: (I i) => [(Char, Instruction i ())]
semantics = [
    ('A', aInstr)
  , ('N', logicalNotInstr)
  , ('O', oInstr)
  , ('X', xInstr)
  ]

aInstr :: (I i) => Instruction i ()
aInstr = op2Instr $ \x y -> if x == 0 then 0 else y

oInstr :: (I i) => Instruction i ()
oInstr = op2Instr $ \x y -> if x /= 0 then x else y

xInstr :: (I i) => Instruction i ()
xInstr = op2Instr $ \x y -> case () of
  _ | x == 0 && y == 0 -> 0
    | x == 0 || y == 0 -> 1
    | otherwise -> 0

