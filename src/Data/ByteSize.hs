module Data.ByteSize (
    ByteSize (..)
  ) where

import Data.Bits
import Data.Int

-----------------------------------------------------------

class ByteSize a where
  byteSize :: a -> Maybe Int

instance ByteSize Integer where
  byteSize _ = Nothing

instance ByteSize Int where
  byteSize _ = Just $ bitSize (0 :: Int) `div` 8

instance ByteSize Int8 where
  byteSize _ = Just $ bitSize (0 :: Int8) `div` 8

instance ByteSize Int16 where
  byteSize _ = Just $ bitSize (0 :: Int16) `div` 8

instance ByteSize Int32 where
  byteSize _ = Just $ bitSize (0 :: Int32) `div` 8

instance ByteSize Int64 where
  byteSize _ = Just $ bitSize (0 :: Int64) `div` 8

