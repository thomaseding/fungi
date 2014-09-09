module Fingerprint.NULL (
    name
  , semantics
  ) where

import Instruction

-----------------------------------------------------------

name :: String
name = "NULL"

semantics :: (I i) => [(Char, Instruction i ())]
semantics = zip ['A'..'Z'] $ repeat reverseInstr

