module Text.Help.Fingerprint.FIXP (
    help
  ) where

import Text.PrintOption

-----------------------------------------------------------

p :: Char -> String -> IO ()
p c = printOption ' ' $ " " ++ [c] ++ " --"

help :: IO ()
help = do
  p 'A' "Pop Y. Pop X. Push the bitwise AND of X and Y."
  p 'B' "Pop X. Push 10000*acos(X/10000). Angle is measured in degrees."
  p 'C' "Pop X. Push 10000*cos(X/10000). Angle is measured in degrees."
  p 'D' "Pop N. Let X be a random number be chosen uniformly from [0, |N|]. If X >= 0 then push N. Else push -N."
  p 'I' "Pop X. Push 10000*sin(X/10000). Angle is measured in degrees."
  p 'J' "Pop X. Push 10000*asin(X/10000). Angle is measured in degrees."
  p 'N' "Pop X. Push -X."
  p 'O' "Pop Y. Pop X. Push the bitwise OR of X and Y."
  p 'P' "Pop X. Push X*pi."
  p 'Q' "Pop X. If X >= 0 then push sqrt(X). Else push X."
  p 'R' "Pop Y. Pop X. Push X^B."
  p 'S' "Pop X. If X > 0 then push 1. If X < 0 then push -1. Else push 0."
  p 'T' "Pop X. Push 10000*tan(X/10000). Angle is measured in degrees."
  p 'U' "Pop X. Push 10000*atan(X/10000). Angle is measured in degrees."
  p 'V' "Pop X. Push |X|."
  p 'X' "Pop Y. Pop X. Push the bitwise XOR of X and Y."

