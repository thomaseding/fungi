module Fingerprint.BZRO (
    name
  , semantics
  ) where

import Control.Monad.State.Strict

import Data.Char (chr, ord)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Tuple.Map

import Space.Cell

import Env
import Instruction
import Ip
import Mode
import qualified Semantics

-----------------------------------------------------------

name :: String
name = "BZRO"

semantics :: (I i) => [(Char, Instruction i ())]
semantics = [
    ('B', bInstr)
  ]

runInstructionInstr :: (I i) => Char -> Instruction i ()
runInstructionInstr c = do
  sem <- gets $ Semantics.removeOverlay Mode.Bizarro . getSemantics . currentIp
  fromMaybe (unknownInstr i) $ Semantics.lookup i sem
  where
    i = ordCell $ charToCell c

bizarroModeInstructions :: (I i) => (i -> Maybe (Instruction i ()), Map i (Maybe (Instruction i ())))
bizarroModeInstructions = (,) f $ buildInstructions $ map (map2 $ Just . runInstructionInstr) [
    ('>', '<')
  , ('<', '>')
  , ('^', 'v')
  , ('v', '^')
  , ('h', 'l')
  , ('l', 'h')
  , ('[', ']')
  , (']', '[')
  , ('_', '|')
  , ('|', '_')
  , ('(', ')')
  , (')', '(')
  , ('{', '}')
  , ('}', '{')
  , ('+', '-')
  , ('-', '+')
  , ('*', '/')
  , ('/', '*')
  , ('i', 'o')
  , ('o', 'i')
  , ('&', '.')
  , ('.', '&')
  , ('~', ',')
  , (',', '~')
  , ('g', 'p')
  , ('p', 'g')
  , ('@', 'q')
  , ('q', '@')
  , ('\'','"')
  , ('"','\'')
  ]
  where
    f x = case cellToChar $ chrCell x of
      Just c
        | c `elem` ['0'..'9'] -> Just $ map runInstructionInstr "fedcba9876" !! (ord c - ord '0')
        | c `elem` ['a'..'f'] -> Just $ map runInstructionInstr "543210" !! (ord c - ord 'a')
        | c `elem` ['A'..'Z'] -> Just $ runInstructionInstr $ chr $ ord 'Z' + ord 'A' - ord c
        | otherwise -> Nothing
      Nothing -> Nothing

bInstr :: (I i) => Instruction i ()
bInstr = do
  modify $ withIp $ toggleMode mode
  modify $ withSemantics $ Semantics.toggleOverlay mode bizarroModeInstructions
  where
    mode = Mode.Bizarro

