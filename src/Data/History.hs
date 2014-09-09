module Data.History (
    History (getSize)
  , empty
  , fromHistory
  , fromList
  , toList
  , push
  , pop
  , popN
  , lookup
  ) where

import Prelude hiding (lookup)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe, mapMaybe)

import Text.PrettyShow

-----------------------------------------------------------

data History a = History {
    getSize :: Int
  , getReplacePos :: Int
  , getMap :: IntMap a
  }
  deriving
    (Show)

instance (PrettyShow a) => PrettyShow (History a) where
  pshow = ("History " ++) . pshow . toList

empty :: Int -> History a
empty n = fromList (Just n) []

fromHistory :: Maybe Int -> History a -> History a
fromHistory Nothing h = h
fromHistory (Just newSize) h
  | newSize >= getSize h = h { getSize = newSize }
  | otherwise = fromList (Just newSize) $ take newSize $ toList h -- TODO : Make this more efficient if needed

fromList :: Maybe Int -> [a] -> History a
fromList mSize xs = foldr push History { 
    getSize = size
  , getReplacePos = 0
  , getMap = IntMap.empty
  } xs
  where
    size = fromMaybe (length xs) mSize

getHeadPos :: History a -> Int
getHeadPos h = if rpos - 1 < 0
  then getSize h - 1
  else rpos - 1
  where
    rpos = getReplacePos h

fromLogicalPos :: History a -> Int -> Int
fromLogicalPos h pos = if pos <= hpos
  then hpos - pos
  else size + hpos - pos
  where
    size = getSize h
    hpos = getHeadPos h

push :: a -> History a -> History a
push x h = h {
    getMap = IntMap.insert rpos x $ getMap h
  , getReplacePos = if rpos + 1 < getSize h
      then rpos + 1
      else 0
  }
  where
    rpos = getReplacePos h

pop :: History a -> History a
pop h = h {
    getMap = IntMap.delete (getHeadPos h) $ getMap h
  , getReplacePos = if rpos - 1 < 0
      then getSize h - 1
      else rpos - 1
  }
  where
    rpos = getReplacePos h

popN :: Int -> History a -> History a
popN n = head . drop n . iterate pop

lookup :: Int -> History a -> Maybe a
lookup pos h
  | 0 <= pos && pos < size = IntMap.lookup (fromLogicalPos h pos) $ getMap h
  | otherwise = Nothing
  where
    size = getSize h

toList :: History a -> [a]
toList h = mapMaybe (`lookup` h) [0 .. getSize h - 1]



