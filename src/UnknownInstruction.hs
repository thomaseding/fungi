module UnknownInstruction (
    UnknownInstruction (..)
  ) where

-----------------------------------------------------------

data UnknownInstruction
  = ReverseUnknown
  | FailUnknown
  | DebugUnknown
  deriving (Show, Eq, Ord)

