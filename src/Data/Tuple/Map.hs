module Data.Tuple.Map (
    map1
  , map2
  ) where

import Data.Tuple.Select
import Data.Tuple.Update

-----------------------------------------------------------

map1 :: (Sel1 a1 a, Upd1 b a1 c) => (a -> b) -> a1 -> c
map1 f t = upd1 (f $ sel1 t) t

map2 :: (Sel2 a1 a, Upd2 b a1 c) => (a -> b) -> a1 -> c
map2 f t = upd2 (f $ sel2 t) t

