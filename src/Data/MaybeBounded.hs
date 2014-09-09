module Data.MaybeBounded (
    MaybeBounded (..)
  ) where

import Data.Int

-----------------------------------------------------------

class MaybeBounded a where
  maybeMaxBound :: Maybe a
  maybeMinBound :: Maybe a

instance MaybeBounded Integer where
  maybeMaxBound = Nothing
  maybeMinBound = Nothing

instance MaybeBounded Int where
  maybeMaxBound = Just maxBound
  maybeMinBound = Just minBound

instance MaybeBounded Int8 where
  maybeMaxBound = Just maxBound
  maybeMinBound = Just minBound

instance MaybeBounded Int16 where
  maybeMaxBound = Just maxBound
  maybeMinBound = Just minBound

instance MaybeBounded Int32 where
  maybeMaxBound = Just maxBound
  maybeMinBound = Just minBound

instance MaybeBounded Int64 where
  maybeMaxBound = Just maxBound
  maybeMinBound = Just minBound

