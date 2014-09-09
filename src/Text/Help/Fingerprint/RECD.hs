module Text.Help.Fingerprint.RECD (
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
  p 'C' "Clear all recordings from the current IP."
  p 'N' "Push the length of the current IP's recordings."
  p 'L' $ "Toggle Learn mode. If Learn mode was on and turns off, the number of additional recorded instructions"
    ++ " is pushed onto the stack."
  p 'R' $ "Toggle Record mode. If Record mode was on and turns off, the number of additional recorded instructions"
    ++ " is pushed onto the stack."
  p 'Q' $ "Pop P. Pop N. Let L = the length of the current IP's recordings. If N < 0 or P > L, do nothing."
    ++ " Otherwise select the IP's first min(N,L-P) recorded instructions after dropping max(P,0) of them and"
    ++ " execute them by the current IP. The IP does not automatically advance between"
    ++ " any of the executed instructions. The (Q) instruction takes a single tick."
  p 'P' $ "Pop P. Pop N. Let L = the length of the current IP's recordings. If N < 0 or P > L, do nothing."
    ++ " Otherwise select the IP's first min(N,L-P) recorded instructions after dropping max(P,0) of them and"
    ++ " execute them by the current IP. In between each executed instruction, the IP advances automatically."
    ++ " The phrase 'In between' should be taken literally, for the IP does not automatically advance after the last"
    ++ " executed instruction. That being said, the IP naturally advances after the (P) instruction completes."
    ++ " The (P) instruction takes a single tick."
  line
  putStrLn "\n ** LEARN MODE **\n"
  putStrLn $ fit maxWidth $ "While in Learn mode, all the RECD instructions except (N) and (L) act as if they reflect."
    ++ " Instead of executing the current instruction, the IP records the instruction. Circumstantial data, such"
    ++ " as the IP's state (stack, position, modes, delta, etc.), are not recorded. These recordings are appended"
    ++ " to previous recordings, if any. Learn mode and Record mode share recordings."
  line
  putStrLn "\n ** Record MODE **\n"
  putStrLn $ fit maxWidth $ "While in Record mode, all the RECD instructions except (N) and (R) act as if they reflect."
    ++ " In addition to executing the current instruction, the IP records the instruction. Circumstantial data, such"
    ++ " as the IP's state (stack, position, modes, delta, etc.), are not recorded. These recordings are appended"
    ++ " to previous recordings, if any. Learn mode and Record mode share recordings."

