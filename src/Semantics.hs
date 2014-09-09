module Semantics (
    Semantics
  , mkSemantics
  , lookup
  , lookupBase
  , lookupFinger
  , lookupOverlay
  , pushFingerprint
  , popFingerprint
  , toggleOverlay
  , addOverlay
  , removeOverlay
  ) where

import Prelude hiding (lookup)

import Control.Monad.State.Strict

import Data.Char (ord)
import Data.I
import Data.Labeled
import Data.Map (Map)
import qualified Data.Map as Map

import Mode

-----------------------------------------------------------

type Instruction env i = StateT (env i) IO

type InfMap k v = (k -> Maybe v, Map k (Maybe v))

-----------------------------------------------------------

data Semantics env i = S {
    baseInstrs :: Map i (Instruction env i ())
  , fingerInstrs :: Map i [Instruction env i ()]
  , overlayInstrs :: [Labeled Mode (InfMap i (Instruction env i ()))]
  }

mkSemantics :: (I i) => Map i (Instruction env i ()) -> Semantics env i
mkSemantics base = S {
    baseInstrs = base
  , fingerInstrs = Map.fromList $ zip [fromIntegral $ ord c | c <- ['A'..'Z']] $ repeat []
  , overlayInstrs = []
  }

lookup :: (I i) => i -> Semantics env i -> Maybe (Instruction env i ())
lookup i sem = case lookupOverlay i sem of
  Just instr -> Just instr
  Nothing -> case lookupFinger i sem of
    Just instr -> Just instr
    Nothing -> lookupBase i sem

lookupOverlay :: (I i) => i -> Semantics env i -> Maybe (Instruction env i ())
lookupOverlay i = lookupOverlay' i . map unlabel . overlayInstrs

lookupFinger :: (I i) => i -> Semantics env i -> Maybe (Instruction env i ())
lookupFinger i = lookupFinger' i . fingerInstrs

lookupBase :: (I i) => i -> Semantics env i -> Maybe (Instruction env i ())
lookupBase i = lookupBase' i . baseInstrs

lookupOverlay' :: (I i) => i -> [InfMap i (Instruction env i ())] -> Maybe (Instruction env i ())
lookupOverlay' _ [] = Nothing
lookupOverlay' i ((f, m) : ms) = case Map.lookup i m of
  Just mInstr -> case mInstr of
    Just instr -> Just instr
    Nothing -> lookupOverlay' i ms
  Nothing -> case f i of
    Just instr -> Just instr
    Nothing -> lookupOverlay' i ms

lookupFinger' :: (I i) => i -> Map i [Instruction env i ()] -> Maybe (Instruction env i ())
lookupFinger' i m = case Map.lookup i m of
  Nothing -> Nothing
  Just [] -> Nothing
  Just (instr:_) -> Just instr

lookupBase' :: (I i) => i -> Map i (Instruction env i ()) -> Maybe (Instruction env i ())
lookupBase' = Map.lookup

pushFingerprint :: (I i) => [(i, Instruction env i ())] -> Semantics env i -> Semantics env i
pushFingerprint assocs sem = sem { fingerInstrs = m' }
  where
    m = fingerInstrs sem
    m' = foldr add m assocs
    add (i, instr) = Map.adjust (instr:) i

popFingerprint :: (I i) => [(i, Instruction env i ())] -> Semantics env i -> Semantics env i
popFingerprint assocs sem = sem { fingerInstrs = m' }
  where
    m = fingerInstrs sem
    m' = foldr (remove . fst) m assocs
    remove = Map.adjust tail

addOverlay :: (I i)
        => Mode
        -> (i -> Maybe (Instruction env i ()), Map i (Maybe (Instruction env i ())))
        -> Semantics env i
        -> Semantics env i
addOverlay mode f_m sem = if mode `elem` map getLabel (overlayInstrs sem)
  then sem
  else addOverlay' mode f_m sem

addOverlay' :: (I i)
        => Mode
        -> (i -> Maybe (Instruction env i ()), Map i (Maybe (Instruction env i ())))
        -> Semantics env i
        -> Semantics env i
addOverlay' mode f_m sem = sem { overlayInstrs = imap : overlayInstrs sem }
  where
    imap = label mode f_m

removeOverlay :: (I i) => Mode -> Semantics env i -> Semantics env i
removeOverlay mode sem = sem { overlayInstrs = removeOverlay' mode $ overlayInstrs sem }

removeOverlay' :: (I i)
        => Mode
        -> [Labeled Mode (InfMap i (Instruction env i ()))]
        -> [Labeled Mode (InfMap i (Instruction env i ()))]
removeOverlay' _ [] = []
removeOverlay' mode (m:ms) = if getLabel m == mode
  then ms
  else m : removeOverlay' mode ms

toggleOverlay :: (I i)
        => Mode
        -> (i -> Maybe (Instruction env i ()), Map i (Maybe (Instruction env i ())))
        -> Semantics env i
        -> Semantics env i
toggleOverlay mode f_m sem = if mode `elem` map getLabel (overlayInstrs sem)
  then removeOverlay mode sem
  else addOverlay' mode f_m sem

