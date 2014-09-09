module Text.Help.Fingerprint.NOP (
    help
  ) where

import Text.PrintOption

-----------------------------------------------------------

p :: Char -> String -> IO ()
p c = printOption ' ' $ " " ++ [c] ++ " --"

help :: IO ()
help = mapM_ (flip p "Does nothing. Like the 'z' instruction.") ['A'..'Z']

