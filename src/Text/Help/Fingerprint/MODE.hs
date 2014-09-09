module Text.Help.Fingerprint.MODE (
    help
  ) where

import Text.PrintOption

-----------------------------------------------------------

p :: Char -> String -> IO ()
p c = printOption ' ' $ " " ++ [c] ++ " --"

help :: IO ()
help = do
  p 'H' "Toggle Hover mode for the current IP. In Hover mode, the instructions '>', '<', '^', 'v', '|', and '_' treat the IP's delta relatively. That is, instead of setting its dx to 1 and the rest of its delta to 0, '>' would instead simply add 1 to its dx."
  p 'I' "Toggle Invert mode for the current IP. When Invert mode is active, cells are pushed on the stack onto the bottom instead of the top."
  p 'Q' "Toggle Queue mode for the current IP. When Queue mode is active, cells are popped off the stack from the bottom instead of the top."
  p 'S' "Toggle Switch mode. In Switch mode, the pairs of instructions '[' and ']', '{' and '}', and '(' and ')' are treated as switches. When one is executed, the cell it is located in is immediately overwritten with the other instruction of the pair, providing a switching mechanism and a way to seperate coincident IPs."

