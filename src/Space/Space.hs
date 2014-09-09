module Space.Space (
    Space
  , mkSpace
  , cellAt
  , putCell
  , travelBy
  , minMaxCoords
  , putSpaceAt
  , inBounds
  ) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector

import Space.Cell

import Text.PrettyShow

-----------------------------------------------------------

data Conservative a = Precise !a | Imprecise !a
  deriving (Show, Eq, Ord)

isPrecise :: Conservative a -> Bool
isPrecise (Precise _) = True
isPrecise _ = False

toImprecise :: Conservative a -> Conservative a
toImprecise (Precise x) = Imprecise x
toImprecise c = c

conservativeVal :: Conservative a -> a
conservativeVal (Precise x) = x
conservativeVal (Imprecise x) = x

instance Functor Conservative where
  fmap f (Precise x) = Precise $ f x
  fmap f (Imprecise x) = Imprecise $ f x

instance (PrettyShow a) => PrettyShow (Conservative a) where
  pshow (Precise x) = "Precise " ++ pshow x
  pshow (Imprecise x) = "Imprecise" ++ pshow x

-----------------------------------------------------------

type CellMap i = Map [i] (Cell i)

data Space i = Space {
    spaceMap :: CellMap i
  , spaceDim :: Int
  , conservativeMinMaxCoords :: !(Conservative (Vector i, Vector i))
  }
  deriving (Show, Eq)

instance (PrettyShow i, Integral i) => PrettyShow (Space i) where
  pshow s = concat [ []
    , "(Space"
    , " "
    , "(min,max)=" ++ pshow (conservativeMinMaxCoords s)
    , " "
    , "assocs=" ++ pshow (Map.assocs . spaceMap $ s)
    , ")"
    ]

mkSpace :: (Integral i) => Int -> ByteString -> Space i
mkSpace dim str
  | dim <= 0  = error "Space.Space.mkSpace given a non-positive dimension"
  | otherwise = case BS.foldl' f (0, 0, 0, space, '\0') str of
      (_, _, _, s, _) -> s
  where
    origin = mkVector $ replicate dim 0
    space = Space {
        spaceMap = Map.empty
      , spaceDim = dim
      , conservativeMinMaxCoords = Precise (origin, origin)
      }
    f (x, y, z, s, p) c = case c of
      ' ' -> (x + 1, y, z, s, c)
      '\r'-> guardDim 2 (0, y + 1, z, s, c)
      '\n'-> guardDim 2 $ if p == '\r'
        then (x, y, z, s, c)
        else (0, y + 1, z, s, c)
      '\f'-> guardDim 3 (0, 0, z + 1, s, c)
      _   -> let
        v = take dim $ [x, y, z] ++ repeat 0
        s' = putCell s (charToCell c) $ mkVector v
        in (x + 1, y, z, s', c)
      where
        guardDim n t = if dim >= n
          then t
          else (x, y, z, s, c)

cellAt :: (Integral i) => Space i -> Vector i -> Cell i
cellAt s pos = Map.findWithDefault (charToCell ' ') (unVector pos) $ spaceMap s

updateMinMaxCoords :: (Ord i) => (Vector i, Vector i) -> Vector i -> (Vector i, Vector i)
updateMinMaxCoords (minPos, maxPos) pos = (zipWithV min minPos pos, zipWithV max maxPos pos)

putCell :: (Num i, Ord i) => Space i -> Cell i -> Vector i -> Space i
putCell s c pos
  | charToCell ' ' == c = if strictInBounds s pos
    then s {
        spaceMap = Map.delete (unVector pos) m
      }
    else if inBounds s pos
      then s {
          spaceMap = Map.delete (unVector pos) m
        , conservativeMinMaxCoords = toImprecise $ conservativeMinMaxCoords s
        }
      else s
  | otherwise = s {
        spaceMap = Map.insert (unVector pos) c m
      , conservativeMinMaxCoords = fmap (`updateMinMaxCoords` pos) $ conservativeMinMaxCoords s
      }
  where
    m = spaceMap s

inBounds :: (Ord i) => Space i -> Vector i -> Bool
inBounds s pos = minPos <=~ pos && pos <=~ maxPos
  where
    (<=~) = liftOrd (<=)
    (minPos, maxPos) = conservativeVal $ conservativeMinMaxCoords s

strictInBounds :: (Ord i) => Space i -> Vector i -> Bool
strictInBounds s pos = minPos <~ pos && pos <~ maxPos
  where
    (<~) = liftOrd (<)
    (minPos, maxPos) = conservativeVal $ conservativeMinMaxCoords s

travelBy :: (Integral i) => Vector i -> Vector i -> Space i -> Vector i
(pos `travelBy` delta) s = if inBounds s pos'
  then pos'
  else wrap pos delta s
  where
    dim = spaceDim s
    pos' = takeV dim (pos + delta)

wrap :: (Integral i) => Vector i -> Vector i -> Space i -> Vector i
wrap pos delta s = (delta +) . head . dropWhile (inBounds s) . iterate (subtract delta) $ pos

minMaxCoords :: (Integral i) => Space i -> (Vector i, Vector i, Space i)
minMaxCoords s = if isPrecise $ conservativeMinMaxCoords s
  then (cMinPos, cMaxPos, s)
  else case Map.lookup (unVector cMinPos) m >> Map.lookup (unVector cMaxPos) m of
    Just _ -> (cMinPos, cMaxPos, s)
    Nothing -> let
      (minPos, maxPos) = Map.foldWithKey f (0, 0) $ spaceMap s
      f k _ z = updateMinMaxCoords z $ mkVector k
      s' = s { conservativeMinMaxCoords = Precise (minPos, maxPos) }
      in (minPos, maxPos, s')
  where
    m = spaceMap s
    (cMinPos, cMaxPos) = conservativeVal $ conservativeMinMaxCoords s

putSpaceAt :: (Integral i) => Vector i -> Space i -> Space i -> Space i
putSpaceAt pos baseSpace = Map.foldWithKey f baseSpace . spaceMap
  where
    f pos' cell space = putCell space cell (pos + mkVector pos')

