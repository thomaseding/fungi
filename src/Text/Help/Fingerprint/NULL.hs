module Text.Help.Fingerprint.NULL (
    help
  ) where

import Text.PrintOption

-----------------------------------------------------------

p :: Char -> String -> IO ()
p c = printOption ' ' $ " " ++ [c] ++ " --"

help :: IO ()
help = mapM_ (flip p "Reverse the IP's delta. Like the 'r' instruction.") ['A'..'Z']

