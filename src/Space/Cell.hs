module Space.Cell (
    Cell
  , chrCell
  , ordCell
  , cellToChar
  , cellToPrintableChar
  , charToCell
  ) where

import Data.Char (chr, ord, isPrint)

import Text.PrettyShow

-----------------------------------------------------------

newtype Cell i = Cell { unCell :: i }
  deriving (Show, Eq, Ord)

instance (Integral i) => PrettyShow (Cell i) where
  pshow cell = concat [ ""
    , "(Cell "
    , maybe "'\\???'" show . cellToChar $ cell
    , " "
    , show . ordCell $ cell
    , ")"
    ]

chrCell :: i -> Cell i
chrCell = Cell

ordCell :: Cell i -> i
ordCell = unCell

cellToChar :: (Integral i) => Cell i -> Maybe Char
cellToChar (Cell n) = if minB <= n && n <= maxB
  then Just . chr $ fromIntegral n
  else Nothing
  where
    minB = fromIntegral . ord $ minBound
    maxB = fromIntegral . ord $ maxBound

cellToPrintableChar :: (Integral i) => Cell i -> Maybe Char
cellToPrintableChar cell = cellToChar cell >>= \c -> if isPrint c
  then Just c
  else Nothing

charToCell :: (Num i) => Char -> Cell i
charToCell = Cell . fromIntegral . ord

