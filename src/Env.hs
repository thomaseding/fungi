module Env (
    Env (..)
  , mkEnv
  , withDebugger
  , withIp
  , withIps
  , withSpace
  , withSs
  , withToss
  , withSemantics
  , currentCell
  , currentIp
  , currentSs
  , currentToss
  , addSpawnedIp
  , addBreakPoint
  , removeBreakPoint
  ) where

import Control.Monad.State.Strict

import Data.Deque
import Data.I
import Data.List (genericTake)
import Data.List.Zipper (Zipper)
import qualified Data.List.Zipper as Zipper
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.MaybeBounded
import qualified Data.Set as Set
import Data.Stack
import qualified Data.Stack as Stack
import Data.Vector

import Debug.Debugger

import Space.Cell
import Space.Space

import Ip
import Semantics
import UnknownInstruction


-----------------------------------------------------------

type Instruction i = StateT (Env i) IO

type Fingerprints i = Map Integer [(i, Instruction i ())]

data Env i = Env {
    getIps :: Zipper (Ip Env i)
  , getDebugger :: Debugger Env i
  , getDim :: Int
  , getSpace :: Space i
  , getSpawnedIps :: [Ip Env i]
  , getValidIds :: [i]
  , getUnshowableChar :: Char
  , getUnknownMode :: UnknownInstruction
  , getProgName :: String
  , getFungeArgs :: [String]
  , getFingerprints :: Fingerprints i
  , getValidReferences :: [i]
  , getReferenceMap :: Map i (Vector i)
  }

mkEnv :: (I i) => Int
        -> Space i 
        -> UnknownInstruction 
        -> Debugger Env i 
        -> String 
        -> [String] 
        -> Fingerprints i 
        -> Map i (Instruction i ())
        -> Env i
mkEnv dim space unknownMode debugger progName args fingerprints baseSemantics = Env {
    getIps = Zipper.fromList [mkIp dim ident baseSemantics]
  , getDebugger = debugger
  , getDim = dim
  , getSpace = space
  , getSpawnedIps = []
  , getValidIds = idents
  , getUnshowableChar = '®'
  , getUnknownMode = unknownMode
  , getProgName = progName
  , getFungeArgs = args
  , getFingerprints = fingerprints
  , getValidReferences = uniqueVals
  , getReferenceMap = Map.empty
  }
  where
    ident : idents = uniqueVals

uniqueVals :: (I i) => [i]
uniqueVals = case maybeMaxBound of
  Nothing -> [0 ..]
  Just maxN -> [0 .. maxN] ++ case maybeMinBound of 
    Nothing -> iterate (subtract 1) (-1)
    Just minN -> init [minN .. 0]

currentIp :: Env i -> Ip Env i
currentIp = Zipper.cursor . getIps

setCurrentIp :: Ip Env i -> Env i -> Env i
setCurrentIp ip = withIps $ Zipper.replace ip

withDebugger :: (Debugger Env i -> Debugger Env i) -> Env i -> Env i
withDebugger f env = env { getDebugger = f $ getDebugger env }

withIp :: (Ip Env i -> Ip Env i) -> Env i -> Env i
withIp f env = setCurrentIp (f $ currentIp env) env

withIps :: (Zipper (Ip Env i) -> Zipper (Ip Env i)) -> Env i -> Env i
withIps f env = env { getIps = f $ getIps env }

withSemantics :: (Semantics Env i -> Semantics Env i) -> Env i -> Env i
withSemantics f = withIp $ \ip -> ip { getSemantics = f $ getSemantics ip }

withSpace :: (Space i -> Space i) -> Env i -> Env i
withSpace f env = env { getSpace = f $ getSpace env }

withSs :: (Stack (Deque i) -> Stack (Deque i)) -> Env i -> Env i
withSs f = withIp $ \ip -> ip { getSs = f $ getSs ip }

withToss :: (Deque i -> Deque i) -> Env i -> Env i
withToss f = withSs $ \ss -> case Stack.top ss of
  Just s -> Stack.push (f s) $ Stack.pop ss
  Nothing -> mkStack1 $ f mkDeque

currentSs :: Env i -> Stack (Deque i)
currentSs = getSs . currentIp

currentToss :: Env i -> Deque i
currentToss = fromMaybe mkDeque . Stack.top . currentSs

currentCell :: (Integral i) => Env i -> Cell i
currentCell env = cellAt s pos
  where
    s = getSpace env
    pos = getPos . currentIp $ env

addSpawnedIp :: Ip Env i -> Env i -> Env i
addSpawnedIp ip env = env { getSpawnedIps = ip : getSpawnedIps env }

addBreakPoint :: (I i) => [i] -> Env i -> Env i
addBreakPoint pos env = withDebugger (withBreakPoints $ Set.insert pos') env
  where
    pos' = genericTake (getDim env) $ pos ++ repeat 0

removeBreakPoint :: (I i) => [i] -> Env i -> Env i
removeBreakPoint pos env = withDebugger (withBreakPoints $ Set.delete pos') env
  where
    pos' = genericTake (getDim env) $ pos ++ repeat 0

