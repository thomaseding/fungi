module Fingerprint.ROMA (
    name
  , semantics
  ) where

import Instruction

-----------------------------------------------------------

name :: String
name = "ROMA"

semantics :: (I i) => [(Char, Instruction i ())]
semantics = [
    ('C', cInstr)
  , ('D', dInstr)
  , ('I', iInstr)
  , ('L', lInstr)
  , ('M', mInstr)
  , ('V', vInstr)
  , ('X', xInstr)
  ]

cInstr :: (I i) => Instruction i ()
cInstr = pushInstr 100

dInstr :: (I i) => Instruction i ()
dInstr = pushInstr 500

iInstr :: (I i) => Instruction i ()
iInstr = pushInstr 1

lInstr :: (I i) => Instruction i ()
lInstr = pushInstr 50

mInstr :: (I i) => Instruction i ()
mInstr = pushInstr 1000

vInstr :: (I i) => Instruction i ()
vInstr = pushInstr 5

xInstr :: (I i) => Instruction i ()
xInstr = pushInstr 10

