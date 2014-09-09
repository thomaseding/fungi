module Text.Help.Fingerprint.REFC (
    help
  ) where

import Data.Int

import Text.PrintOption

-----------------------------------------------------------

p :: Char -> String -> IO ()
p c = printOption ' ' $ " " ++ [c] ++ " --"

maxWidth :: Int
maxWidth = 80

num32BitVals :: Integer
num32BitVals = fromIntegral maxB - fromIntegral minB
  where
    maxB = maxBound :: Int32
    minB = minBound :: Int32

help :: IO ()
help = do
  p 'R' $ "Pop a vector off the stack. Push a scalar onto the stack unique to that vector. If 'R' is called multiple"
    ++ " times with equivalent vectors, each one gets a different unique scalar. Should the interpreter run out"
    ++ " of unique values, an error message will be emitted, and the program will end. The number of unique values"
    ++ " is equivalent to the number of values the funge cell size can acquire. For example, if 32 bit integers"
    ++ " are used, 'R' can be used safely " ++ show num32BitVals ++ " times."
  p 'D' $ "Pop X. If X corresponds to a vector via the 'R' instruction, push that vector onto the stack."
    ++ " Otherwise reverse the current IP (X is still popped)."
  putStrLn $ fit maxWidth $ "Note that the memory used to keep these correspondences are never freed"
    ++ " during the execution of the program. Hence if used enough, it is more likely that the program will fail"
    ++ " due to too much memory being used than not being able to create a new unique value."

