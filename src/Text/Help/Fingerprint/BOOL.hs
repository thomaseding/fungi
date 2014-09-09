module Text.Help.Fingerprint.BOOL (
    help
  ) where

import Text.PrintOption

-----------------------------------------------------------

p :: Char -> String -> IO ()
p c = printOption ' ' $ " " ++ [c] ++ " --"

help :: IO ()
help = do
  p 'A' "Pop Y. Pop X. Push 0 if X is 0. Else push Y."
  p 'N' "Pop X. Push 1 if X is 0. Else push 0."
  p 'O' "Pop Y. Pop X. Push X if X is not 0. Else push Y."
  p 'X' "Pop Y. Pop X. Push 1 if X is 0 and Y not 0 or if X is not 0 and Y is 0. Else push 1."

