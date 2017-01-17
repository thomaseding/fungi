module Ip (
    Ip
  , mkIp

  , getId
  , getPos
  , getDelta
  , getSs
  , isAlive
  , getStorageOffset
  , getSemantics
  , hrtiMark


  , testMode
  , addMode
  , removeMode
  , toggleMode

  , record
  , clearRecordings
  , getRecordings
  , getRecordLength
  , setRecordLength

  , killIp
  , reverseIp
  , setPos
  , setDelta
  , setSs
  , setStorageOffset

  ) where

import Control.Monad.State.Strict

import Data.Deque
import Data.I
import Data.Map (Map)
import Data.StackSet (StackSet)
import qualified Data.StackSet as StackSet
import Data.Sequence
import qualified Data.Sequence as Seq
import Data.Stack
import Data.Vector

import Text.PrettyShow

import Mode
import Semantics

-----------------------------------------------------------

type Instruction env i = StateT (env i) IO

data Ip env i = Ip {
    getId :: i
  , getPos :: Vector i
  , getDelta :: Vector i
  , getSs :: Stack (Deque i)
  , isAlive :: Bool
  , getStorageOffset :: !(Vector i)
  , getSemantics :: Semantics env i
  , modes :: StackSet Mode
  , hrtiMark :: Maybe Integer
  , getRecordings :: Seq (Instruction env i ())
  , getRecordLength :: i
  }

instance (PrettyShow i) => PrettyShow (Ip env i) where
  pshow ip = concat [ []
    , "(IP"
    , " "
    , "id=" ++ pshow (getId ip)
    , " "
    , "pos=" ++ pshow (getPos ip)
    , " "
    , "delta=" ++ pshow (getDelta ip)
    , " "
    , "modes=" ++ pshow (StackSet.toList $ modes ip)
    , ")"
    ]

mkIp :: (I i) => Int -> i -> Map i (Instruction env i ()) -> Ip env i
mkIp dim ident baseSemantics = Ip {
    getId = ident
  , getPos = takeV dim 0
  , getDelta = takeV dim (1 `cons` 0)
  , getSs = mkStack1 mkDeque
  , isAlive = True
  , getStorageOffset = takeV dim 0
  , getSemantics = mkSemantics baseSemantics
  , modes = StackSet.empty
  , hrtiMark = Nothing
  , getRecordings = Seq.empty
  , getRecordLength = 0
  }

testMode :: Mode -> Ip env i -> Bool
testMode mode = StackSet.member mode . modes

addMode :: Mode -> Ip env i -> Ip env i
addMode mode ip = ip { modes = StackSet.insert mode $ modes ip }

removeMode :: Mode -> Ip env i -> Ip env i
removeMode mode ip = ip { modes = StackSet.delete mode $ modes ip }

toggleMode :: Mode -> Ip env i -> Ip env i
toggleMode mode ip = if testMode mode ip
  then removeMode mode ip
  else addMode mode ip

killIp :: Ip env i -> Ip env i
killIp ip = ip { isAlive = False }

reverseIp :: (Num i) => Ip env i -> Ip env i
reverseIp ip = ip { getDelta = negate . getDelta $ ip }

setPos :: (Num i) => Vector i -> Ip env i -> Ip env i
setPos pos ip = ip { getPos = pos }

setDelta :: (Num i) => Vector i -> Ip env i -> Ip env i
setDelta delta ip = ip { getDelta = delta }

setSs :: Stack (Deque i) -> Ip env i -> Ip env i
setSs s ip = ip { getSs = s }

setStorageOffset :: Vector i -> Ip env i -> Ip env i
setStorageOffset v ip = ip { getStorageOffset = v }

setRecordLength :: i -> Ip env i -> Ip env i
setRecordLength n ip = ip { getRecordLength = n }

clearRecordings :: Ip env i -> Ip env i
clearRecordings ip = ip { getRecordings = Seq.empty }

record :: Instruction env i () -> Ip env i -> Ip env i
record instr ip = ip { getRecordings = getRecordings ip |> instr }

