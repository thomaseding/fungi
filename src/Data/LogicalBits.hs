module Data.LogicalBits (
    logicalBit
  , testLogicalBit
  ) where

import Prelude hiding (fromInteger, toInteger)

import Data.Bits

-----------------------------------------------------------

toInt :: (Integral a) => a -> Int
toInt = fromIntegral

fromInt :: (Integral a) => Int -> a
fromInt = fromIntegral

logicalBit :: (Integral a) => Int -> a
logicalBit = fromInt . bit

testLogicalBit :: (Integral a) => a -> Int -> Bool
testLogicalBit n = testBit $ toInt n

