module Data.I (
    I
  ) where

import Data.Bits
import Data.ByteSize
import Data.Int
import Data.IntegralLike
import Data.MaybeBounded

import Text.PrettyShow

import Random

-----------------------------------------------------------

-- The I type class is intentionally empty. Used to clump the type classes.
class (Bits i, ByteSize i, Integral i, IntegralLike i, MaybeBounded i, PrettyShow i, Random i, Read i) => I i where

instance I Integer where
instance I Int where
instance I Int8 where
instance I Int16 where
instance I Int32 where
instance I Int64 where

