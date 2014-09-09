module Text.Help.Fingerprint.BZRO (
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

correspondences :: [(Char, Char)]
correspondences = [
    ('>', '<')
  , ('<', '>')
  , ('^', 'v')
  , ('v', '^')
  , ('h', 'l')
  , ('l', 'h')
  , ('[', ']')
  , (']', '[')
  , ('_', '|')
  , ('|', '_')
  , ('(', ')')
  , (')', '(')
  , ('{', '}')
  , ('}', '{')
  , ('+', '-')
  , ('-', '+')
  , ('*', '/')
  , ('/', '*')
  , ('i', 'o')
  , ('o', 'i')
  , ('&', '.')
  , ('.', '&')
  , ('~', ',')
  , (',', '~')
  , ('g', 'p')
  , ('p', 'g')
  , ('@', 'q')
  , ('q', '@')
  , ('\'','"')
  , ('"','\'')
  ] ++ zip nums (reverse nums) ++ zip alphas (reverse alphas)
  where
    nums = "0123456789abcdef"
    alphas = ['A'..'Z']

help :: IO ()
help = do
  p 'B' "Toggle Bizarro mode."
  line
  putStrLn "\n ** BIZARRO MODE **\n"
  putStrLn $ fit maxWidth $ "In Bizarro mode, the following instructions on the left correspond to the corresponding"
    ++ " instruction on the right as if Bizarro mode were not on:\n"
  mapM_ (\(x, y) -> putStrLn $ "            " ++ [x] ++ "    --->    " ++ [y]) correspondences
  putStrLn ""
  putStrLn $ fit maxWidth $ "Note that if Bizarro mode is on, a (Y) instruction will"
    ++ " turn off Bizarro mode and (B) will not due to (Y) corresponding to (B) and vice-versa."
    ++ " Also note that the corresponding instructions are not necessarily the base Funge-98"
    ++ " instructions. For example, if Hover mode is and was enabled before Bizarro mode (see MODE fingerprint"
    ++ " for details on Hover mode), (<) will add 1 the IP's first delta coordinate"
    ++ " instead of setting the IP's delta to east (or west for that matter)."
    ++ " As with all modes, the most recent mode has priority. As an example, if Hover mode"
    ++ " is enabled after and while Bizarro mode is enabled, (<) will subtract 1 from the IP's first delta"
    ++ " coordinate. To alleviate any confusion about String mode, if String mode is enabled during Bizarro"
    ++ " mode, a (') instruction will push 39 onto the stack, and (\") will disable String mode despite the"
    ++ " fact that (') turns on String mode when Bizarro mode is on."
  putStrLn $ fit maxWidth $ "\nAll the clarifications above are not special rules. The only rule is that the"
    ++ " instructions correspond to other instructions based on symbols, not on semantics. The clarifications"
    ++ " assume 'usual' Bizarro mode circumstances, meaning some other mode or instruction is not radically"
    ++ " changing the behavior of Bizarro mode, other modes, instructions, and/or the interpreter."

