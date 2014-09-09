module Data.Deque (
    Deque
  , mkDeque
  , mkDeque1
  , isEmpty
  , depth
  , Data.Deque.toList
  , pop
  , popBottom
  , top
  , bottom
  , dig
  , digBottom
  , push
  , pushBottom
  ) where

import qualified Data.Foldable as Foldable
import Data.List (genericDrop, intercalate)
import Data.Sequence
import qualified Data.Sequence as Sequence

import Text.PrettyShow

-----------------------------------------------------------

newtype Deque a = D (Seq a)
  deriving (Show, Eq, Ord)

instance (PrettyShow a) => PrettyShow (Deque a) where
  pshow (D s) = Prelude.concat [ []
    , "["
    , intercalate "," . map pshow $ Foldable.toList s
    , "..]"
    ]

-----------------------------------------------------------

mkDeque :: Deque a
mkDeque = D empty

mkDeque1 :: a -> Deque a
mkDeque1 = D . singleton

isEmpty :: Deque a -> Bool
isEmpty (D s) = Sequence.null s

depth :: Deque a -> Integer
depth (D s) = Foldable.foldl' (\l _ -> 1 + l) 0 s

toList :: Deque a -> [a]
toList (D s) = Foldable.toList s

pop :: Deque a -> Deque a
pop (D s) = case viewl s of
  _ :< s' -> D s'
  EmptyL -> D empty

popBottom :: Deque a -> Deque a
popBottom (D s) = case viewr s of
  s' :> _ -> D s'
  EmptyR -> D empty

top :: Deque a -> Maybe a
top (D s) = case viewl s of
  x :< _ -> Just x
  EmptyL -> Nothing

bottom :: Deque a -> Maybe a
bottom (D s) = case viewr s of
  _ :> x -> Just x
  EmptyR -> Nothing

dig :: (Integral i) => i -> Deque a -> Maybe a
dig n = top . head . genericDrop n . iterate pop

digBottom :: (Integral i) => i -> Deque a -> Maybe a
digBottom n = top . head . genericDrop n . iterate popBottom

push :: a -> Deque a -> Deque a
push x (D s) = D (x <| s)

pushBottom :: a -> Deque a -> Deque a
pushBottom x (D s) = D (s |> x)


