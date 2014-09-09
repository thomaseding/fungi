module Fingerprint.REFC (
    name
  , semantics
  ) where

import Control.Monad.State.Strict

import qualified Data.Map as Map

import System.Exit (exitFailure)

import Env
import Instruction

-----------------------------------------------------------

name :: String
name = "REFC"

semantics :: (I i) => [(Char, Instruction i ())]
semantics = [
    ('R', rInstr)
  , ('D', dInstr)
  ]

rInstr :: (I i) => Instruction i ()
rInstr = do
  refs <- gets getValidReferences
  case refs of
    [] -> liftIO $ do
      putStrLn "REFC fingerprint: Cannot create a unique reference for vector."
      exitFailure
    r : rs -> do
      vec <- popDimVectorInstr
      pushInstr r
      refMap <- gets $ Map.insert r vec . getReferenceMap
      modify $ \env -> env { getReferenceMap = refMap, getValidReferences = rs }

dInstr :: (I i) => Instruction i ()
dInstr = do
  r <- popInstr
  gets getReferenceMap >>= maybe reverseInstr pushVectorInstr . Map.lookup r

