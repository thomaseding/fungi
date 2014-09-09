module Interpreter (
    runFungi
  ) where

import Control.Arrow ((&&&))
import Control.Monad.State.Strict

import qualified Data.List.Zipper as Zipper

import System.Exit (ExitCode, exitSuccess)

import Env
import Instruction
import Ip

-----------------------------------------------------------

data RunResult i = RunResult {
    getLiveIps :: [Ip Env i]
  , getResultEnv :: Env i
  }

runFungi :: (I i) => Env i -> IO ExitCode
runFungi = evalStateT $ forever runEnvRound

runEnvRound :: (I i) => Instruction i ()
runEnvRound = do
  noIps <- gets $ Zipper.emptyp . getIps
  if noIps
    then liftIO exitSuccess
    else do
      runResult <- runEnvRound'
      let ips = Zipper.fromList $ getLiveIps runResult
      modify $ \env -> env { getIps = ips }
      advanceIps

runEnvRound' :: (I i) => Instruction i (RunResult i)
runEnvRound' = do
  ips <- gets getIps
  case Zipper.safeCursor ips of
    Nothing -> do
      env <- get
      return RunResult {
          getLiveIps = []
        , getResultEnv = env
        }
    Just _ -> do
      runCurrentInstruction
      (ip, spawnedIps) <- gets $ currentIp &&& getSpawnedIps
      let liveIps = spawnedIps ++ [ip | isAlive ip]
      modify $ \env -> withIps Zipper.right env {
          getSpawnedIps = []
        , getValidIds = [getId ip | not $ isAlive ip] ++ getValidIds env
        }
      runResult <- runEnvRound'
      let liveIps' = liveIps ++ getLiveIps runResult
      return runResult { getLiveIps = liveIps' }

advanceIps :: (I i) => Instruction i ()
advanceIps = do
  ips <- gets getIps
  case Zipper.safeCursor ips of
    Nothing -> modify $ withIps Zipper.start
    Just _ -> do
      trampolineInstr
      modify $ withIps Zipper.right
      advanceIps

