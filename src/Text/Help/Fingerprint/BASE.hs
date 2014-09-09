module Text.Help.Fingerprint.BASE (
    help
  ) where

import Text.PrintOption

-----------------------------------------------------------

p :: Char -> String -> IO ()
p c = printOption ' ' $ " " ++ [c] ++ " --"

help :: IO ()
help = do
  p 'B' "Pop X. Output X in base 2 followed by a space."
  p 'H' "Pop X. Output X in base 16 followed by a space."
  p 'I' "Pop N. Read input in base N."
  p 'N' "Pop X. Pop N. Output X in base N followed by a space."
  p 'O' "Pop X. Output X in base 8 followed by a space."

