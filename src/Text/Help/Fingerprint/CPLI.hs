module Text.Help.Fingerprint.CPLI (
    help
  ) where

import Text.PrintOption

-----------------------------------------------------------

p :: Char -> String -> IO ()
p c = printOption ' ' $ " " ++ [c] ++ " --"

help :: IO ()
help = do
  p 'A' "Pop A. Pop B. Pop C. Pop D. Let E+Fi be the complex number (A+Bi)+(C+Di). Push E. Push F."
  p 'D' "Pop A. Pop B. Pop C. Pop D. Let E+Fi be the complex number (A+Bi)/(C+Di). Push E. Push F."
  p 'M' "Pop A. Pop B. Pop C. Pop D. Let E+Fi be the complex number (A+Bi)*(C+Di). Push E. Push F."
  p 'O' "Pop A. Pop B. Output the complex number (A+Bi) followed by a space."
  p 'S' "Pop A. Pop B. Pop C. Pop D. Let E+Fi be the complex number (A+Bi)-(C+Di). Push E. Push F."
  p 'V' "Pop A. Pop B. Push |A+Bi|. Note |A+Bi| = sqrt(A^2+B^2)."

