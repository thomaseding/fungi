module Fingerprint.NOP (
    name
  , semantics
  ) where

import Instruction

-----------------------------------------------------------

name :: String
name = "NOP"

semantics :: (I i) => [(Char, Instruction i ())]
semantics = zip ['A'..'Z'] $ repeat nopInstr

