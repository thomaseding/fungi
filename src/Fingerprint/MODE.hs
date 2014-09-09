module Fingerprint.MODE (
    name
  , semantics
  ) where

import Control.Monad.State.Strict

import Data.Map (Map)
import Data.Vector

import Space.Cell
import Space.Space

import Env
import Instruction
import Ip
import qualified Mode
import qualified Semantics

-----------------------------------------------------------

name :: String
name = "MODE"

semantics :: (I i) => [(Char, Instruction i ())]
semantics = [
    ('H', hInstr)
  , ('I', iInstr)
  , ('Q', qInstr)
  , ('S', sInstr)
  ]

-----------------------------------------------------------

addDeltaInstr :: (I i) => [i] -> Instruction i ()
addDeltaInstr ds = do
  env <- get
  let dim = getDim env
      delta = getDelta $ currentIp env
      delta' = unVector $ delta + mkVector (take dim $ ds ++ repeat 0)
  setDeltaInstr delta'

hoverGoEastInstr :: (I i) => Instruction i ()
hoverGoEastInstr = addDeltaInstr [1]

hoverGoWestInstr :: (I i) => Instruction i ()
hoverGoWestInstr = addDeltaInstr [-1]

hoverGoNorthInstr :: (I i) => Instruction i ()
hoverGoNorthInstr = guardDim 2 $ addDeltaInstr [0, -1]

hoverGoSouthInstr :: (I i) => Instruction i ()
hoverGoSouthInstr = guardDim 2 $ addDeltaInstr [0, 1]

hoverEastWestIfInstr :: (I i) => Instruction i ()
hoverEastWestIfInstr = ifInstr hoverGoWestInstr hoverGoEastInstr

hoverNorthSouthIfInstr :: (I i) => Instruction i ()
hoverNorthSouthIfInstr = guardDim 2 $ ifInstr hoverGoNorthInstr hoverGoSouthInstr

hoverModeInstructions :: (I i) => (i -> Maybe (Instruction i ()), Map i (Maybe (Instruction i ())))
hoverModeInstructions = (,) (const Nothing) $ buildInstructions [
    ('>', Just hoverGoEastInstr)
  , ('<', Just hoverGoWestInstr)
  , ('^', Just hoverGoNorthInstr)
  , ('v', Just hoverGoSouthInstr)
  , ('|', Just hoverNorthSouthIfInstr)
  , ('_', Just hoverEastWestIfInstr)
  ]

-----------------------------------------------------------

setCurrentCellInstr :: (I i) => Char -> Instruction i ()
setCurrentCellInstr c = do
  pos <- gets $ getPos . currentIp
  modify $ withSpace $ \s -> putCell s (charToCell c) pos

switchModeInstructions :: (I i) => (i -> Maybe (Instruction i ()), Map i (Maybe (Instruction i ())))
switchModeInstructions = (,) (const Nothing) $ buildInstructions [
    ('[', Just $ setCurrentCellInstr ']' >> turnLeftInstr)
  , (']', Just $ setCurrentCellInstr '[' >> turnRightInstr)
  , ('{', Just $ setCurrentCellInstr '}' >> beginBlockInstr)
  , ('}', Just $ setCurrentCellInstr '{' >> endBlockInstr)
  , ('(', Just $ setCurrentCellInstr ')' >> loadSemanticsInstr)
  , (')', Just $ setCurrentCellInstr '(' >> unloadSemanticsInstr)
  ]

-----------------------------------------------------------

hInstr :: (I i) => Instruction i ()
hInstr = do
  modify $ withIp $ toggleMode mode
  modify $ withSemantics $ Semantics.toggleOverlay mode hoverModeInstructions
  where
    mode = Mode.Hover

iInstr :: (I i) => Instruction i ()
iInstr = modify $ withIp $ toggleMode Mode.Invert

qInstr :: (I i) => Instruction i ()
qInstr = modify $ withIp $ toggleMode Mode.Queue

sInstr :: (I i) => Instruction i ()
sInstr = do
  modify $ withIp $ toggleMode mode
  modify $ withSemantics $ Semantics.toggleOverlay mode switchModeInstructions
  where
    mode = Mode.Switch

