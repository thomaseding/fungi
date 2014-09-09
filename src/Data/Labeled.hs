module Data.Labeled (
    Labeled (getLabel, unlabel)
  , label
  ) where

-----------------------------------------------------------

data Labeled l a = Labeled {
    getLabel :: l
  , unlabel :: a
  }
  deriving (Show)

label :: l -> a -> Labeled l a
label = Labeled

instance Functor (Labeled l) where
  fmap f l = l { unlabel = f $ unlabel l }

