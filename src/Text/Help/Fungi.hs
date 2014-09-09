module Text.Help.Fungi (
    help
  ) where

import Data.ByteSize (byteSize)
import Data.Char (chr, toLower)
import Data.Int
import Data.List (sort, intercalate, nub)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import System.Environment (getProgName)
import System.FilePath (dropExtension)

import Text.PrintOption

import Fingerprint

-----------------------------------------------------------

p :: Char -> String -> String -> IO ()
p = printOption

b :: Char -> String -> String -> IO ()
b = printOptionWith defaultPrintSettings { bulletDelim = Just "***" }

supportedSizes :: String
supportedSizes = intercalate ", " $ map show $ sort $ nub $ map (fromMaybe 0) [
    byteSize (0 :: Integer)
  , byteSize (0 :: Int)
  , byteSize (0 :: Int8)
  , byteSize (0 :: Int16)
  , byteSize (0 :: Int32)
  , byteSize (0 :: Int64)
  ]

fingers :: String
fingers = intercalate ", " $ sort $ map (reverse . decode) $ Map.keys (fingerprints :: Fingerprints Integer)
  where
    decode x = if x == 0
      then ""
      else chr (fromInteger m) : decode d
      where
        (d, m) = x `divMod` 256

line :: IO ()
line = putStrLn $ replicate maxWidth '-'

maxWidth :: Int
maxWidth = 80

help :: IO ()
help = do
  line
  progName <- fmap (map toLower . dropExtension) getProgName
  putStrLn $ "Usage: " ++ progName ++ " [OPTIONS] [PROGRAM FILE] [PROGRAM ARGS]"
  line
  putStrLn $ "Options:"
  p '?' "help" "Display this help message."
  p ' ' "version" "Display version."
  p ' ' "finger-doc NAME" "Display documentation for the fingerprint with name NAME."
  b 'd' "debug [MODE=1]" $ concat [
      "Run program using debugger."
    , "***MODE=1,on,true: Run debugger in step mode."
    , "***MODE=0,off,false: Run without debugger."
    , "\nDefault is off."
    ]
  p 's' "cell-size SIZE" $ concat [
      "Set the funge cell byte size to SIZE.\n"
    , "Supported sizes are " ++ supportedSizes ++ ".\n"
    , "If SIZE <= 0 then cell size is unbounded.\n"
    , "Default is " ++ show (fromMaybe 0 $ byteSize (0 :: Int)) ++ "."
    ]
  b 'n' "dim DIM" $ concat [
      "Set the funge dimensions to DIM."
    , "\nDefault depends on file extension:"
    , "***uf u98: 1"
    , "***bf b98: 2"
    , "***tf t98: 3"
    , "\nAll other extensions default to 2."
    , "\nAllowed values: 0 < DIM <= " ++ show (maxBound :: Int) ++ "."
    ]
  b 'u' "unknown MODE" $ concat [
      "What to do when encountering an unknown instruction."
    , "***MODE=reverse: Reverse on unknown instruction."
    , "***MODE=debug: Launch debugger on unknown instruction. Instruction reverses."
    , "***MODE=fail: Terminate program on unknown instruction."
    , "\nDefault is reverse."
    ]
  line
  p ' ' "FINGERPRINTS--" fingers
  line
  putStrLn $ fit maxWidth $ "There is also the +RTS option if Fungi was compiled via the GHC"
    ++ " (Glaskow Haskell Compiler). This option deals with run time options of the program and is not determined"
    ++ " by Fungi. Using this option may improve performance of Fungi. However, this option"
    ++ " (and options related to +RTS) is beyond the scope of this program. See GHC documentation"
    ++ " for more details."
  line

