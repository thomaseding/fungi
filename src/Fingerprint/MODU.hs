module Fingerprint.MODU (
    name
  , semantics
  ) where

import Instruction

-----------------------------------------------------------

name :: String
name = "MODU"

semantics :: (I i) => [(Char, Instruction i ())]
semantics = [
    ('M', mInstr)
  , ('U', uInstr)
  , ('R', rInstr)
  ]

mInstr :: (I i) => Instruction i ()
mInstr = op2Instr $ guardZero mod

uInstr :: (I i) => Instruction i ()
uInstr = op2Instr $ guardZero (\x -> abs . mod x)

rInstr :: (I i) => Instruction i ()
rInstr = op2Instr $ guardZero rem

