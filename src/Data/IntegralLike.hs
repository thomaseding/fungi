module Data.IntegralLike (
    IntegralLike (..)
  ) where

import Data.Char (ord)
import Data.Int

-----------------------------------------------------------

class IntegralLike a where
  asIntegral :: (Integral i) => a -> i

instance IntegralLike Bool where
  asIntegral False = 0
  asIntegral True = 1

instance IntegralLike Char where
  asIntegral = fromIntegral . ord

instance IntegralLike Integer where
  asIntegral = fromInteger

instance IntegralLike Int where
  asIntegral = fromIntegral

instance IntegralLike Int8 where
  asIntegral = fromIntegral

instance IntegralLike Int16 where
  asIntegral = fromIntegral

instance IntegralLike Int32 where
  asIntegral = fromIntegral

instance IntegralLike Int64 where
  asIntegral = fromIntegral

