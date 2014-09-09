module Data.Stack (
    Stack
  , mkStack
  , mkStack1
  , isEmpty
  , depth
  , toList
  , pop
  , top
  , dig
  , push
  ) where

import Data.List (genericDrop, genericLength, intercalate)

import Text.PrettyShow

-----------------------------------------------------------

newtype Stack a = S [a]
  deriving (Show, Eq, Ord)

instance (PrettyShow a) => PrettyShow (Stack a) where
  pshow (S xs) = concat [ []
    , "["
    , intercalate "," . map pshow $ xs
    , "..]"
    ]

-----------------------------------------------------------

mkStack :: Stack a
mkStack = S []

mkStack1 :: a -> Stack a
mkStack1 x = S [x]

isEmpty :: Stack a -> Bool
isEmpty (S []) = True
isEmpty _      = False

depth :: Stack a -> Integer
depth (S xs) = genericLength xs

toList :: Stack a -> [a]
toList (S xs) = xs

pop :: Stack a -> Stack a
pop (S xs) = case xs of
  _ : ys -> S ys
  [] -> S []

top :: Stack a -> Maybe a
top (S xs) = case xs of
  y : _ -> Just y
  [] -> Nothing

dig :: (Integral i) => i -> Stack a -> Maybe a
dig n = top . head . genericDrop n . iterate pop

push :: a -> Stack a -> Stack a
push x (S xs) = S (x : xs)

