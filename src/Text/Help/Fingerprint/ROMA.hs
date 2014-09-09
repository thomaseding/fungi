module Text.Help.Fingerprint.ROMA (
    help
  ) where

import Text.PrintOption

-----------------------------------------------------------

p :: Char -> String -> IO ()
p c = printOption ' ' $ " " ++ [c] ++ " --"

help :: IO ()
help = do
  p 'C' "Push 100 onto the stack."
  p 'D' "Push 500 onto the stack."
  p 'I' "Push 1 onto the stack."
  p 'L' "Push 50 onto the stack."
  p 'M' "Push 1000 onto the stack."
  p 'V' "Push 5 onto the stack."
  p 'X' "Push 10 onto the stack."

