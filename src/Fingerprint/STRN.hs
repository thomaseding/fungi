module Fingerprint.STRN (
    name
  , semantics
  ) where

import Control.Monad.State.Strict

import Data.Char (isDigit, isSpace)
import Data.IntegralLike
import Data.List (tails, isPrefixOf, genericTake, genericDrop, genericLength, foldl')
import Data.Maybe (fromMaybe, listToMaybe)
import Data.MaybeBounded
import Data.Vector

import Space.Cell
import Space.Space

import System.IO (hFlush, stdout)

import Env
import Instruction
import Ip

-----------------------------------------------------------

name :: String
name = "STRN"

semantics :: (I i) => [(Char, Instruction i ())]
semantics = [
    ('A', aInstr)
  , ('C', cInstr)
  , ('D', dInstr)
  , ('F', fInstr)
  , ('G', gInstr)
  , ('I', iInstr)
  , ('L', lInstr)
  , ('M', mInstr)
  , ('N', nInstr)
  , ('P', pInstr)
  , ('R', rInstr)
  , ('S', sInstr)
  , ('V', vInstr)
  ]

pushStringInstr :: (I i) => [i] -> Instruction i ()
pushStringInstr str = do
  pushInstr 0
  pushVectorInstr $ mkVector $ reverse str

aInstr :: (I i) => Instruction i ()
aInstr = do
  s1 <- popStringInstr'
  s2 <- popStringInstr'
  pushStringInstr $ s1 ++ s2

cInstr :: (I i) => Instruction i ()
cInstr = do
  s1 <- popStringInstr'
  s2 <- popStringInstr'
  pushInstr $ case compare s1 s2 of
    GT -> 1
    LT -> -1
    EQ -> 0

dInstr :: (I i) => Instruction i ()
dInstr = do
  s <- liftM (map $ fromMaybe '?') popStringInstr
  liftIO $ putStr s

search :: (Eq a) => [a] -> [a] -> [a]
search needle = fromMaybe [] . listToMaybe . filter (needle `isPrefixOf`) . tails

fInstr :: (I i) => Instruction i ()
fInstr = do
  s1 <- popStringInstr'
  s2 <- popStringInstr'
  pushStringInstr $ search s2 s1

gInstr :: (I i) => Instruction i ()
gInstr = do
  env <- get
  let dim = getDim env
      space = getSpace env
      east = takeV dim $ 1 `cons` 0
      so = getStorageOffset $ currentIp env
      (minC, _, space') = minMaxCoords space
      minX = head $ unVector minC
      wrappingAdd v w  = let z = v + w
        in if inBounds space' z
          then z
          else minX `cons` (dropV 1 z)
  pos <- popDimVectorInstr
  let poses = iterate (wrappingAdd east) $ pos + so
      cells = map (cellAt space) poses
      vals = map ordCell cells
      str = takeWhile (/= 0) vals
  modify $ withSpace $ const space'
  pushStringInstr str

iInstr :: (I i) => Instruction i ()
iInstr = do
  str <- liftIO $ do
    hFlush stdout
    getLine
  pushStringInstr $ map asIntegral str

lInstr :: (I i) => Instruction i ()
lInstr = do
  n <- popInstr
  str <- popStringInstr'
  pushStringInstr $ genericTake n str

mInstr :: (I i) => Instruction i ()
mInstr = do
  n <- popInstr
  p <- popInstr
  str <- popStringInstr'
  pushStringInstr $ genericTake n $ genericDrop p str

nInstr :: (I i) => Instruction i ()
nInstr = do
  str <- popStringInstr'
  pushStringInstr str -- needs to be done like this because of Hover mode
  pushInstr $ genericLength str

pInstr :: (I i) => Instruction i ()
pInstr = do
  env <- get
  let dim = getDim env
      space = getSpace env
      east = takeV dim $ 1 `cons` 0
      so = getStorageOffset $ currentIp env
  pos <- popDimVectorInstr
  str <- liftM (++ [0]) popStringInstr'
  let cells = map chrCell str
      poses = iterate (east +) $ pos + so
      space' = foldl' (\s -> uncurry $ putCell s) space $ zip cells poses
  modify $ withSpace $ const space'

rInstr :: (I i) => Instruction i ()
rInstr = do
  n <- popInstr
  str <- popStringInstr'
  pushStringInstr $ reverse $ genericTake n $ reverse str

sInstr :: (I i) => Instruction i ()
sInstr = popInstr >>= pushStringInstr . map asIntegral . show

atoi :: (MaybeBounded i, Integral i) => String -> i
atoi str = n''
  where
    n'' = fromInteger $ maybe n' (min n' . fromIntegral) $ maybeMaxBound `asTypeOf` Just n''
    n' = maybe n (max n . fromIntegral) $ maybeMinBound `asTypeOf` Just n''
    n = case dropWhile isSpace str of
      "" -> 0
      '+':cs -> atoi' cs
      '-':cs -> negate $ atoi' cs
      cs -> atoi' cs

atoi' :: String -> Integer
atoi' = foldl' (\n c -> 10 * n + read [c]) 0 . takeWhile isDigit

vInstr :: (I i) => Instruction i ()
vInstr = do
  str <- liftM (map $ fromMaybe '?') popStringInstr
  pushInstr $ atoi str 

