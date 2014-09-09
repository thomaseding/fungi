module Text.Help.Fingerprint.ORTH (
    help
  ) where

import Text.PrintOption

-----------------------------------------------------------

p :: Char -> String -> IO ()
p c = printOption ' ' $ " " ++ [c] ++ " --"

help :: IO ()
help = do
  p 'A' "Pop Y. Pop X. Push the bitwise AND of X and Y."
  p 'O' "Pop Y. Pop X. Push the bitwise OR of X and Y."
  p 'E' "Pop Y. Pop X. Push the bitwise XOR of X and Y."
  p 'X' "Pop X. Set the IP's position's x coordinate to X."
  p 'Y' "Pop Y. Set the IP's position's y coordinate to Y."
  p 'V' "Pop X. Set the IP's dx to X."
  p 'W' "Pop Y. Set the IP's dy to Y."
  p 'G' $ "If the dimension < 2 then reverse. Otherwise do the following: Pop Y. Pop X. Retrieve the value in"
    ++ " funge space at the coordinates (X,Y). Push that value. If the dimension > 2, then the higher"
    ++ " dimension coordinates are 0."
  p 'P' $ "If the dimension < 2 then reverse. Otherwise do the following: Pop Y. Pop X. Pop V. Place the value V in"
    ++ " funge space at the coordinates (X,Y). If the dimension > 2, then the higher dimension coordinates are 0."
  p 'Z' "Pop X. If X = 0, then trampoline (as in a '#' instruction). Else do nothing (as in a 'z' instruction)."
  p 'S' "Output a 0 terminated string: Pop X. If X = 0 then do nothing. Else output X as a character and repeat."

