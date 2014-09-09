module Random (
    Random (..)
  ) where

import Control.Monad

import Data.Int

import System.Random.MWC (GenIO, Variate)
import qualified System.Random.MWC as MWC

-----------------------------------------------------------

class Random a where
  uniformR :: (a, a) -> IO a

-----------------------------------------------------------

uniformR' :: Variate a => (a, a) -> GenIO -> IO a
uniformR' = MWC.uniformR

uniformRIO :: Variate a => (a, a) -> IO a
uniformRIO = MWC.withSystemRandom . uniformR'

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)

fromInt64 :: Int64 -> Integer
fromInt64 = fromIntegral

instance Random Integer where
  uniformR = liftM fromInt64 . uniformRIO . mapPair fromInteger

instance Random Int where
  uniformR = uniformRIO

instance Random Int8 where
  uniformR = uniformRIO

instance Random Int16 where
  uniformR = uniformRIO

instance Random Int32 where
  uniformR = uniformRIO

instance Random Int64 where
  uniformR = uniformRIO

