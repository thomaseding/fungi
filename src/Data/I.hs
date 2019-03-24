{-# LANGUAGE ConstraintKinds #-}

module Data.I (
    I
  ) where

import Data.Bits
import Data.ByteSize
import Data.IntegralLike
import Data.MaybeBounded

import Text.PrettyShow

import Random

-----------------------------------------------------------

type I i = (Show i, Bits i, ByteSize i, Integral i, IntegralLike i, MaybeBounded i, PrettyShow i, Random i, Read i)

