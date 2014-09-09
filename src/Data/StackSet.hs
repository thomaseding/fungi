module Data.StackSet (
    StackSet
  , empty
  , toList
  , insert
  , delete
  , member
  ) where

import Data.Labeled
import Data.List (sortBy)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Ord (comparing)

-----------------------------------------------------------

newtype Tagged l a = T { untag :: Labeled l a }

instance (Eq a) => Eq (Tagged l a) where
  T l1 == T l2 = unlabel l1 == unlabel l2

instance (Ord a) => Ord (Tagged l a) where
  compare (T l1) (T l2) = comparing unlabel l1 l2

type Tag = Int

data StackSet a = S !Tag (Set (Tagged Tag a))

empty :: StackSet a
empty = S maxBound Set.empty

toList :: StackSet a -> [a]
toList (S _ set) = map (unlabel . untag) $ sortBy (comparing $ getLabel . untag) $ Set.toList set

insert :: (Ord a) => a -> StackSet a -> StackSet a
insert x stackSet@(S n _) = if n > minBound
  then insert' x stackSet'
  else insert' x $ retag stackSet'
  where
    stackSet' = delete x stackSet

insert' :: (Ord a) => a -> StackSet a -> StackSet a
insert' x (S n set) = if n > minBound
  then S (n - 1) $ Set.insert (T $ label n x) set
  else error "StackSet.insert: Too many elements in stackset!"

retag :: (Ord a) => StackSet a -> StackSet a
retag = foldr insert' empty . toList

dummyTag :: a -> Tagged Tag a
dummyTag x = T $ label undefined x

delete :: (Ord a) => a -> StackSet a -> StackSet a
delete x (S n set) = S n $ Set.delete (dummyTag x) set

member :: (Ord a) => a -> StackSet a -> Bool
member x (S _ set) = Set.member (dummyTag x) set

