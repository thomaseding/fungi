module Text.PrettyShow (
    PrettyShow (..)
  , pprint
  ) where

import Data.Int

-----------------------------------------------------------

class PrettyShow a where
  pshow :: a -> String
  pshowList :: [a] -> String
  pshowList [] = "[]"
  pshowList (x:xs) = '[' : pshow x ++ pshowl xs
    where
      pshowl [] = "]"
      pshowl (y:ys) = ',' : pshow y ++ pshowl ys

instance (PrettyShow a) => PrettyShow [a] where
  pshow = pshowList

instance PrettyShow Bool where
  pshow = show

instance PrettyShow Char where
  pshow = show
  pshowList = flip showList ""
  
instance PrettyShow Integer where
  pshow = show

instance PrettyShow Int where
  pshow = show

instance PrettyShow Int8 where
  pshow = show

instance PrettyShow Int16 where
  pshow = show

instance PrettyShow Int32 where
  pshow = show

instance PrettyShow Int64 where
  pshow = show

instance (PrettyShow a, PrettyShow b) => PrettyShow (a, b) where
  pshow (a, b) = "(" ++ pshow a ++ "," ++ pshow b ++ ")"

pprint :: (PrettyShow a) => a -> IO ()
pprint = putStrLn . pshow

