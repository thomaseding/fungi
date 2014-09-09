module Debug.Debugger (
    Debugger (..)
  , DebugMode (..)
  , withBreakPoints
  , withWatchExprs
  , withHistory
  ) where

import Control.Monad.State.Strict

import Data.History
import Data.Set (Set)

-----------------------------------------------------------

type Instruction env i = StateT (env i) IO

-----------------------------------------------------------

data DebugMode = DebugStep | DebugContinue | DebugOff
  deriving (Show, Eq, Ord)

data Debugger env i = Debugger {
    getDebugMode :: DebugMode
  , runDebugger :: Instruction env i ()
  , getBreakPoints :: Set [i]
  , getWatchExprs :: Set [i]
  , getHistory :: !(History (env i))
  , getLocaleRads :: (Int, Int)
  }

withBreakPoints :: (Set [i] -> Set [i]) -> Debugger env i -> Debugger env i
withBreakPoints f d = d { getBreakPoints = f $ getBreakPoints d }

withWatchExprs :: (Set [i] -> Set [i]) -> Debugger env i -> Debugger env i
withWatchExprs f d = d { getWatchExprs = f $ getWatchExprs d }

withHistory :: (History (env i) -> History (env i)) -> Debugger env i -> Debugger env i
withHistory f d = d { getHistory = f $ getHistory d } 

