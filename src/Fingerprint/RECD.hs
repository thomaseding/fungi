module Fingerprint.RECD (
    name
  , semantics
  ) where

import Control.Monad.State.Strict

import Data.Foldable (foldl', toList)
import Data.List (genericTake, genericDrop, intersperse)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)

import Env
import Instruction
import Ip
import Mode
import Semantics

-----------------------------------------------------------

name :: String
name = "RECD"

semantics :: (I i) => [(Char, Instruction i ())]
semantics = [
    ('C', cInstr)
  , ('L', lInstr)
  , ('N', nInstr)
  , ('R', rInstr)
  , ('P', pInstr)
  , ('Q', qInstr)
  ]

recordingMode :: Mode
recordingMode = Mode.Record

learnInstructions :: (I i) => (i -> Maybe (Instruction i ()), Map i (Maybe (Instruction i ())))
learnInstructions = (,) (Just . learnInstr_) $ buildInstructions [
    ('L', Just lInstr)
  ]

recordInstructions :: (I i) => (i -> Maybe (Instruction i ()), Map i (Maybe (Instruction i ())))
recordInstructions = (,) (Just . recordInstr) $ buildInstructions [
    ('R', Just rInstr)
  ]

learnInstr_ :: (I i) => i -> Instruction i ()
learnInstr_ i = learnInstr i >> return ()

learnInstr :: (I i) => i -> Instruction i (Instruction i ())
learnInstr i = do
  sem <- gets $ Semantics.removeOverlay recordingMode . getSemantics . currentIp
  let instr = fromMaybe (unknownInstr i) $ Semantics.lookup i sem
  modify $ withIp $ record instr
  recordLen <- gets $ getRecordLength . currentIp
  modify $ withIp $ setRecordLength $! recordLen + 1
  return instr

recordInstr :: (I i) => i -> Instruction i ()
recordInstr = join . learnInstr

cInstr :: (I i) => Instruction i ()
cInstr = do
  recording <- gets $ testMode Mode.Record . currentIp
  learning <- gets $ testMode Mode.Learn . currentIp
  if recording || learning
    then reverseInstr
    else modify $ withIp clearRecordings

seqLength :: (Integral i) => Seq a -> i
seqLength = foldl' (\n _ -> n + 1) 0

nInstr :: (I i) => Instruction i ()
nInstr = gets (seqLength . getRecordings . currentIp) >>= pushInstr

lInstr :: (I i) => Instruction i ()
lInstr = do
  recording <- gets $ testMode Mode.Record . currentIp
  if recording
    then reverseInstr
    else do
      learning <- gets $ testMode Mode.Learn . currentIp
      when learning $ do
        recordLen <- gets $ getRecordLength . currentIp
        modify $ withIp $ setRecordLength 0
        pushInstr recordLen
      modify $ withIp $ toggleMode Mode.Learn
      modify $ withSemantics $ Semantics.toggleOverlay recordingMode learnInstructions

rInstr :: (I i) => Instruction i ()
rInstr = do
  learning <- gets $ testMode Mode.Learn . currentIp
  if learning
    then reverseInstr
    else do
      recording <- gets $ testMode Mode.Record . currentIp
      when recording $ do
        recordLen <- gets $ getRecordLength . currentIp
        modify $ withIp $ setRecordLength 0
        pushInstr recordLen
      modify $ withIp $ toggleMode Mode.Record
      modify $ withSemantics $ Semantics.toggleOverlay recordingMode recordInstructions

getRecordingsInstr :: (I i) => Instruction i [Instruction i ()]
getRecordingsInstr = do
  idx <- popInstr
  n <- popInstr
  let takeN = if n >= 0
        then genericTake n
        else id
  gets (takeN . genericDrop idx . toList . getRecordings . currentIp) 

pInstr :: (I i) => Instruction i ()
pInstr = do
  recording <- gets $ testMode Mode.Record . currentIp
  learning <- gets $ testMode Mode.Learn . currentIp
  if recording || learning
    then reverseInstr
    else do
      modify $ withIp $ addMode Mode.Learn . addMode Mode.Record
      getRecordingsInstr >>= sequence_ . intersperse trampolineInstr
      modify $ withIp $ removeMode Mode.Learn . removeMode Mode.Record

qInstr :: (I i) => Instruction i ()
qInstr = do
  recording <- gets $ testMode Mode.Record . currentIp
  learning <- gets $ testMode Mode.Learn . currentIp
  if recording || learning
    then reverseInstr
    else do
      modify $ withIp $ addMode Mode.Learn . addMode Mode.Record
      getRecordingsInstr >>= sequence_
      modify $ withIp $ removeMode Mode.Learn . removeMode Mode.Record

