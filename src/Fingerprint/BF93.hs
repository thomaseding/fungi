module Fingerprint.BF93 (
    name
  , semantics
  ) where

import Prelude hiding (lookup)

import Control.Monad.State.Strict

import Data.Map (Map)

import System.Exit (exitWith, ExitCode (ExitSuccess))

import Env
import Instruction
import Ip
import Mode
import Semantics

-----------------------------------------------------------

name :: String
name = "BF93"

semantics :: (I i) => [(Char, Instruction i ())]
semantics = [
    ('B', bInstr)
  ]

string93ModeInstructions :: (I i) => (i -> Maybe (Instruction i ()), Map i (Maybe (Instruction i ())))
string93ModeInstructions = (,) (Just . pushInstr) $ buildInstructions [
    ('"', Just string93ModeInstr)
  ]

string93ModeInstr :: (I i) => Instruction i ()
string93ModeInstr = do
  modify $ withIp $ toggleMode mode
  modify $ withSemantics $ Semantics.toggleOverlay mode string93ModeInstructions
  where
    mode = Mode.String93

exitInstr :: (I i) => Instruction i ()
exitInstr = liftIO $ exitWith ExitSuccess

befunge93ModeInstructions :: (I i) => (i -> Maybe (Instruction i ()), Map i (Maybe (Instruction i ())))
befunge93ModeInstructions = (,) (const $ Just reverseInstr) $ buildInstructions [
    ('"', Just string93ModeInstr)
  , ('@', Just exitInstr)
  , (' ', Just spaceInstr)
  , ('+', Just addInstr)
  , ('-', Just subtractInstr)
  , ('*', Just multiplyInstr)
  , ('/', Just divideInstr)
  , ('%', Just remainderInstr)
  , ('`', Just greaterThanInstr)
  , ('>', Just goEastInstr)
  , ('<', Just goWestInstr)
  , ('^', Just goNorthInstr)
  , ('v', Just goSouthInstr)
  , ('?', Just goAwayInstr)
  , ('_', Just eastWestIfInstr)
  , ('|', Just northSouthIfInstr)
  , (':', Just duplicateInstr)
  , ('\\',Just swapInstr)
  , ('$', Just popInstr_)
  , ('.', Just outputDecimalInstr)
  , (',', Just outputCharacterInstr)
  , ('#', Just trampolineInstr)
  , ('g', Just getInstr)
  , ('p', Just putInstr)
  , ('&', Just inputDecimalInstr)
  , ('~', Just inputCharacterInstr)
  , ('!', Just logicalNotInstr)
  ]

bInstr :: (I i) => Instruction i ()
bInstr = do
  modify $ withIp $ toggleMode mode
  modify $ withSemantics $ Semantics.toggleOverlay mode befunge93ModeInstructions
  where
    mode = Mode.Befunge93

