module Data.Vector (
    Vector
  , unVector
  , mkVector
  , takeV
  , dropV
  , reverseV
  , zipWithV
  , foldrV
  , cons
  , append
  , liftOrd
  ) where

import Data.List (intercalate)

import Text.PrettyShow

-----------------------------------------------------------

newtype Vector a = Vector { unVector :: [a] }
  deriving (Show, Eq)

instance (PrettyShow a) => PrettyShow (Vector a) where
  pshow (Vector xs) = '(' : intercalate "," (map pshow xs) ++ ")"

instance Functor Vector where
  fmap f = Vector . map f . unVector

instance (Num a) => Num (Vector a) where
  fromInteger = Vector . repeat . fromInteger
  negate = fmap negate
  signum = fmap signum
  abs = fmap abs
  (+) = zipWithV' (+)
  (-) = zipWithV' (-)
  (*) = zipWithV' (*)

mkVector :: [a] -> Vector a
mkVector = Vector

liftOrd :: (a -> a -> Bool) -> (Vector a -> Vector a -> Bool)
liftOrd f xs = and . unVector . zipWithV f xs

dropV :: Int -> Vector a -> Vector a
dropV n = mkVector . drop n . unVector

takeV :: Int -> Vector a -> Vector a
takeV n = mkVector . take n . unVector

reverseV :: Vector a -> Vector a
reverseV = mkVector . reverse . unVector

zipWithV :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWithV f (Vector xs) (Vector ys) = Vector $ zipWith f xs ys

zipWithV' :: (Num a, Num b) => (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWithV' f (Vector xs) (Vector ys) = Vector $ zipWith' f xs ys

zipWith' :: (Num a, Num b) => (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] [] = []
zipWith' f (x:xs) [] = f x 0 : zipWith' f xs []
zipWith' f [] (y:ys) = f 0 y : zipWith' f [] ys
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

foldrV :: (a -> b -> b) -> b -> Vector a -> b
foldrV f z = foldr f z . unVector

infixr 5 `cons`, `append`
cons :: a -> Vector a -> Vector a
x `cons` (Vector xs) = Vector (x : xs)

append :: Vector a -> Vector a -> Vector a
append xs ys = foldrV cons ys xs

