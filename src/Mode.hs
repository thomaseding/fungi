module Mode (
    Mode (..)
  ) where

import Text.PrettyShow

-----------------------------------------------------------

data Mode
  = Befunge93
  | Bizarro
  | Hover
  | Invert
  | Learn
  | Record
  | String
  | String93
  | Switch
  | Queue
  deriving (Show, Eq, Ord)

instance PrettyShow Mode where
  pshow = show

