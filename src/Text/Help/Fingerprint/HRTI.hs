module Text.Help.Fingerprint.HRTI (
    help
  ) where

import Text.PrintOption

-----------------------------------------------------------

p :: Char -> String -> IO ()
p c = printOption ' ' $ " " ++ [c] ++ " --"

help :: IO ()
help = do
  p 'G' $ "Push the smallest clock tick the underlying system can reliably handle, measured in microseconds."
    ++ " Results may vary from call to call."
  p 'M' "Mark the current IP with a timestamp of the current time."
  p 'T' $ "If the current IP has been marked by 'M', push the number of microseconds between the current"
    ++ " time and the marked time. Otherwise reverse the IP."
  p 'E' "If the current IP has been marked by 'M', remove the mark."
  p 'S' "Push the number of microseconds since the last whole second."

