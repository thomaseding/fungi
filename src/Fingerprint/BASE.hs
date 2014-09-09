module Fingerprint.BASE (
    name
  , semantics
  ) where

import Control.Monad.State.Strict

import Data.Char (chr, ord, toLower)
import Data.Maybe (isNothing)
import Data.MaybeBounded

import Numeric (showIntAtBase)

import System.IO (stdin, stdout, hFlush)
import System.IO.Buffering (BufferMode (..), withBuffering)

import Instruction

-----------------------------------------------------------

name :: String
name = "BASE"

semantics :: (I i) => [(Char, Instruction i ())]
semantics = [
    ('B', outputBaseInstr 2)
  , ('H', outputBaseInstr 16)
  , ('I', inputBaseInstr)
  , ('N', popInstr >>= outputBaseInstr)
  , ('O', outputBaseInstr 8)
  ]

intToDigit :: Int -> Char
intToDigit i
  | 0 <= i && i <= 9 = chr $ ord '0' + i
  | otherwise = chr $ ord 'a' - 10 + i

outputBaseInstr :: (I i) => i -> Instruction i ()
outputBaseInstr base
  | 2 <= base && base <= 36 = do
      n <- popInstr
      outcome <- tryLiftIO $ do
        when (n < 0) $ putChar '-'
        putStr $ showIntAtBase base intToDigit (abs n) " "
      when (isNothing outcome) reverseInstr
  | otherwise = popInstr >> reverseInstr

inputBaseInstr :: (I i) => Instruction i ()
inputBaseInstr = do
  base <- popInstr
  if 2 <= base && base <= 36
    then do
      outcome <- tryLiftIO $ do
        hFlush stdout
        withBuffering NoBuffering stdin $ const $ getBase base
      maybe reverseInstr pushInstr outcome
    else reverseInstr  

toBase :: (Integral i) => i -> Char -> Maybe Integer
toBase base c
  | '0' <= c && c <= '9' = checkSize $ fromIntegral $ ord c - ord '0'
  | 'a' <= c' && c' <= 'z' = checkSize $ fromIntegral $ ord c' + 10 - ord 'a'
  | otherwise = Nothing
  where
    c' = toLower c
    checkSize n = if n < fromIntegral base
      then Just n
      else Nothing

getBase :: (I i) => i -> IO i
getBase base = do
  c <- getChar
  case toBase base c of
    Just k -> case maybeMaxBound `asTypeOf` Just base of
      Just bound -> if k > fromIntegral bound
        then getBase base
        else getBase' base k
      Nothing -> getBase' base k
    Nothing -> getBase base

getBase' :: (I i) => i -> Integer -> IO i
getBase' base n = do
  c <- getChar
  case toBase base c of
    Just k -> let
      n' = fromIntegral base * n + k
      in case maybeMaxBound `asTypeOf` Just base of
        Just bound -> if n' > fromIntegral bound
          then return $ fromIntegral n
          else getBase' base n'
        Nothing -> getBase' base n'
    Nothing -> return $ fromIntegral n

