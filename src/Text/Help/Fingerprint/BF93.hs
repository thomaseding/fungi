module Text.Help.Fingerprint.BF93 (
    help
  ) where

import Data.List (intercalate)

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
  p 'B' "Toggle Befunge93 mode."
  line
  putStrLn "\n ** BEFUNGE93 MODE **\n"
  putStrLn "In Befunge93 mode, the following instructins take on new meaning: "
  p '"' $ "Toggle String93 mode. Like ordinary String mode, except contiguous spaces are not"
    ++ " collapsed. Instead, each space is pushed onto the stack."
  p '@' $ "Exit the program with exit code 0 regardless of the number of live IPs in the program."
  putStrLn $ fit maxWidth $ "In addition, the following instructions behave as the base Funge-98 instructions:\n"
    ++ (intercalate ", " $ map (\c -> '(' : c : ")") " +-*/%!`><^v?_|:\\$.,#gp&~") ++ ".\n"
    ++ "Every other instruction reflects. Thus there is no way for an IP to leave Befunge93 mode"
    ++ " via the BF93 fingerprint."

