module Text.Help.Fingerprint.STRN (
    help
  ) where

import Text.PrintOption

-----------------------------------------------------------

p :: Char -> String -> IO ()
p c = printOption ' ' $ " " ++ [c] ++ " --"

maxWidth :: Int
maxWidth = 80

line :: IO ()
line = putStrLn $ replicate maxWidth '-'

help :: IO ()
help = do
  putStrLn $ fit maxWidth $ "All strings described by this fingerprint (STRN) are 0 terminated strings."
    ++ " By default, let variables of the form S or S# be strings. Strings are stored on the stack such that"
    ++ " the terminating 0 is popped last. Let |S| be defined to the the length of S"
    ++ " excluding the terminating 0. Note that strings are simply sequences of integer values."
  line
  p 'A' "Pop S1. Pop S2. Push the string formed by appending S2 to S1."
  p 'C' $ "Pop S1. Pop S2. Push 1 if S1 > S2. Push -1 if S1 < S2. Otherwise push 0. These comparisons are"
    ++ " done lexicographically."
  p 'D' $ "Output the string as Unicode characters to the standard output. Any numbers that cannot be converted"
    ++ " to Unicode characters are displayed as question marks ('?')."
  p 'F' $ "Pop S1. Pop S2. While S2 is not a prefix of S1 and S1 is not the empty string, drop a value from S1."
    ++ " Push the resulting string S1."
  p 'G' $ "Pop a vector V. Read a string from funge space beginning at V, heading east, until a 0 is encountered."
    ++ " The read string may wrap across the edge of space exactly as an IP would normally wrap with a delta of east."
    ++ " Push the read string."
  p 'I' $ "Read a line from the standard input. Push the resulting string onto the stack. The newline is not part of"
    ++ " the string."
  p 'L' $ "Pop N. Pop S. If N < 0, push the empty string. If N > |S|, push S. Otherwise push the string composed of"
    ++ " the first N values of S."
  p 'M' $ "Pop N. Pop P. Pop S. If N < 0 or P > |S|, push the empty string. If P < 0, let Z = S. Otherwise let Z be"
    ++ " the string S but with the first P characters removed. If N > |Z|, push Z. Otherwise push the string"
    ++ " consisting of the first N characters of Z."
  p 'N' "Pop S. Push S. Push |S|."
  p 'P' $ "Pop S. Pop a vector V. Store the string S in funge space at beginning at position V, heading east."
    ++ " Note that the terminating 0 is stored in funge space."
  p 'R' $ "Pop N. Pop S. If N < 0, push the empty string. If N > |S|, push S. Otherwise push the string"
    ++ " composed of the last N values of S."
  p 'S' $ "Pop N. Push the ASCII string representation of N. Note that if N < 0, a '-' (45) will be pushed last."
    ++ " If N >= 0, a '+' (43) is NOT pushed at all."
  p 'V' $ "Pop S. Let Z be the string S but with leading (Latin-1) whitespace stripped. Read an integer from Z,"
    ++ " optionally signed with a '+' (43) or a '-' (45). The reading is done until a value outside of '0'-'9'"
    ++ " (48-57) is encountered. Let N be the resulting integer or 0 if nothing can be read. If the cell size"
    ++ " is bounded, N will be appropriately constrained by the bounds. (For example, if the S = \"2147483648\","
    ++ "  N will be 2147483647, not -2147483648 if the cell size is 32 bits.) Push N."

