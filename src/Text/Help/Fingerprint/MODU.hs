module Text.Help.Fingerprint.MODU (
    help
  ) where

import Text.PrintOption

-----------------------------------------------------------

p :: Char -> String -> IO ()
p c = printOption ' ' $ " " ++ [c] ++ " --"

help :: IO ()
help = do
  p 'M' $ "Pop Y. Pop X. Push the integer remainder, satisfying [(X `quot` Y)*Y + (X `rem` Y) == X],"
    ++ " where quot is integer division truncated toward zero."
  p 'U' "Same as 'M', but the pushes the absolute value of the result instead of its signed value."
  p 'R' $ "Pop Y. Pop X. Push the integer modulus, satisfying [(X `div` Y)*Y + (X `mod` Y) == X],"
    ++ " where div is integer division truncated toward negative infinity."

