module Fungi (
    main
  , mycology
  ) where

import Control.Exception (handle)
import Control.Monad

import Data.ByteSize (byteSize)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS (hGetContents)
import qualified Data.History as History
import Data.Int
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

import Debug.Debug
import Debug.Debugger

import Space.Space

import System (system)
import System.Environment (getArgs, withArgs)
import System.Exit (ExitCode (..))
import System.FilePath (takeExtension)
import System.IO hiding (hGetContents)

import qualified Text.Help.Fingerprint
import qualified Text.Help.Fungi

import Env
import Fingerprint
import Instruction
import Interpreter
import ProcessArgs
import qualified Version

-----------------------------------------------------------

mycology :: IO ExitCode
mycology = do
  removeTemps
  ret <- withArgs ["--unknown", "debug", "tests/mycology/mycology.b98"] main
  removeTemps
  return ret
  where
    removeTemps = system "rm tests/mycology/mycotmp*.tmp" >> return ()

main :: IO ExitCode
main = do
  mArgs <- liftM processArgs getArgs
  case mArgs of
    Nothing -> badArgs >> return (ExitFailure 1)
    Just args
      | argsHelp args -> Text.Help.Fungi.help >> return ExitSuccess
      | argsVersion args -> version >> return ExitSuccess
      | otherwise -> case argsFingerDoc args of
          Just fingerName -> Text.Help.Fingerprint.help fingerName >> return ExitSuccess
          Nothing -> case argsFile args of
            Nothing -> usage >> return ExitSuccess
            Just file -> readSourceFile file >>= startFungi args file

readSourceFile :: FilePath -> IO ByteString
readSourceFile file = withFile file ReadMode $ \hdl -> BS.hGetContents hdl

runFirst :: (Monad m) => [m (Maybe a)] -> m (Maybe a)
runFirst [] = return Nothing
runFirst (m:ms) = m >>= maybe (runFirst ms) (return . Just)

startFungi :: ProcessedArgs -> FilePath -> ByteString -> IO ExitCode
startFungi args file fileContents = do
  ret <- runFirst [
      whenSize Nothing $ runFungi (env :: Env Integer)
    , whenSize (byteSize (0 :: Int)) $ runFungi (env :: Env Int)
    , whenSize (byteSize (0 :: Int8)) $ runFungi (env :: Env Int8)
    , whenSize (byteSize (0 :: Int16)) $ runFungi (env :: Env Int16)
    , whenSize (byteSize (0 :: Int32)) $ runFungi (env :: Env Int32)
    , whenSize (byteSize (0 :: Int64)) $ runFungi (env :: Env Int64)
    ]
  maybe (usage >> return (ExitFailure 1)) return ret
  where
    dim = fromMaybe (detectDim file) $ argsDim args
    debugger :: (I i) => Debugger Env i
    debugger = Debugger {
        getDebugMode = argsDebugMode args
      , Debug.Debugger.runDebugger = Debug.Debug.runDebugger
      , getBreakPoints = Set.empty
      , getWatchExprs = Set.empty
      , getHistory = History.empty 0
      , getLocaleRads = (7, 3)
      }
    space :: (I i) => Space i
    space = mkSpace dim fileContents
    env :: (I i) => Env i
    env = mkEnv dim space (argsUnknownMode args) debugger file (argsFungeArgs args) fingerprints baseInstructions
    whenSize size action = if size == argsCellByteSize args
      then fmap Just $ handle return action
      else return Nothing

detectDim :: FilePath -> Int
detectDim file = case takeExtension file of
  ".uf" -> 1
  ".u98" -> 1
  ".bf" -> 2
  ".b98" -> 2
  ".tf" -> 3
  ".t98" -> 3
  _ -> 2

usage :: IO ()
usage = putStrLn "See --help for usage."

version :: IO ()
version = do
  putStrLn $ "Fungi version " ++ Version.version

badArgs :: IO ()
badArgs = putStrLn "Bad arguments." >> usage

