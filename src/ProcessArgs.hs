module ProcessArgs (
    processArgs
  , ProcessedArgs (..)
  ) where

import Control.Monad

import Data.ByteSize (byteSize)
import Data.Char (toLower)

import Debug.Debugger (DebugMode (..))

import UnknownInstruction

-----------------------------------------------------------

data ProcessedArgs = ProcessedArgs {
    argsHelp :: Bool
  , argsVersion :: Bool
  , argsFingerDoc :: Maybe String
  , argsDebugMode :: DebugMode
  , argsCellByteSize :: Maybe Int
  , argsDim :: Maybe Int
  , argsUnknownMode :: UnknownInstruction
  , argsFile :: Maybe FilePath
  , argsFungeArgs :: [String]
  }
  deriving (Show, Eq, Ord)

defaultProcessedArgs :: ProcessedArgs
defaultProcessedArgs = ProcessedArgs {
    argsHelp = False
  , argsVersion = False
  , argsFingerDoc = Nothing
  , argsDebugMode = DebugOff
  , argsCellByteSize = byteSize (0 :: Int)
  , argsDim = Nothing
  , argsUnknownMode = ReverseUnknown
  , argsFile = Nothing
  , argsFungeArgs = []
  }

processArgs :: [String] -> Maybe ProcessedArgs
processArgs [] = Just defaultProcessedArgs
processArgs (arg:args) = case arg of
  "--help" -> processHelp args
  "-?" -> processHelp args
  "--version" -> processVersion args
  "--finger-doc" -> processFingerDoc args
  "--debug" -> processDebug args
  "-d" -> processDebug args
  "--cell-size" -> processCellSize args
  "-s" -> processCellSize args
  "--dim" -> processDim args
  "-n" -> processDim args
  "--unknown" -> processUknown args
  "-u" -> processUknown args
  _ -> processFile arg args

processHelp :: [String] -> Maybe ProcessedArgs
processHelp args = processArgs args >>= \p -> return p { argsHelp = True }

processVersion :: [String] -> Maybe ProcessedArgs
processVersion args = processArgs args >>= \p -> return p { argsVersion = True }

processFingerDoc :: [String] -> Maybe ProcessedArgs
processFingerDoc [] = Nothing
processFingerDoc (arg:args) = processArgs args >>= \p -> return p { argsFingerDoc = Just arg }

processUknown :: [String] -> Maybe ProcessedArgs
processUknown [] = Nothing
processUknown (arg:args) = case mUnknownMode of
  Nothing -> Nothing
  Just unknownMode -> processArgs args >>= \p -> return p { argsUnknownMode = unknownMode }
  where
    mUnknownMode = case map toLower arg of
      "reverse" -> Just ReverseUnknown
      "fail" -> Just FailUnknown
      "debug" -> Just DebugUnknown
      _ -> Nothing

processDebug :: [String] -> Maybe ProcessedArgs
processDebug [] = processArgs [] >>= \p -> return p { argsDebugMode = DebugStep }
processDebug (arg:args) = case mDebugMode of
  Nothing -> processArgs (arg:args) >>= \p -> return p { argsDebugMode = DebugStep }
  Just debugMode -> processArgs args >>= \p -> return p { argsDebugMode = debugMode }
  where
    mDebugMode = case map toLower arg of
      "0" -> Just DebugOff
      "1" -> Just DebugStep
      "false" -> Just DebugOff
      "true" -> Just DebugStep
      "off" -> Just DebugOff
      "on" -> Just DebugStep
      _ -> Nothing

processDim :: [String] -> Maybe ProcessedArgs
processDim [] = Nothing
processDim (arg:args) = case reads arg of
  [(n, "")] -> processArgs args >>= \p -> return p { argsDim = Just n }
  _ -> Nothing

processCellSize :: [String] -> Maybe ProcessedArgs
processCellSize [] = Nothing
processCellSize (arg:args) = case reads arg of
  [(n, "")] -> processArgs args >>= \p -> return p { argsCellByteSize = guard (n > 0) >> Just n }
  _ -> Nothing

processFile :: FilePath -> [String] -> Maybe ProcessedArgs
processFile file args = processArgs [] >>= \p -> return p {
    argsFile = Just file
  , argsFungeArgs = args
  }

