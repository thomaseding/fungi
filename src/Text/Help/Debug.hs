module Text.Help.Debug (
    help
  ) where

import Text.PrintOption

-----------------------------------------------------------

p :: Char -> String -> String -> IO ()
p = printOption

help :: IO ()
help = do
  putStrLn "Note: Help commands do not need '-' characters in front of them."
  putStrLn "Values of the form POS are space separated numbers."
  putStrLn "Values of the form VALS are space separated VAL constructs."
  putStrLn "Values of the form VAL can be any one of the following:"
  putStrLn "    - A number."
  putStrLn "    - A single non-quote character."
  putStrLn "    - A quoted set of characters. Single and double quotes allowed."
  putStrLn "-----------------------------------------------------------"
  p '?' "help" "Displays this message."
  p ' ' "exit" "Exit the program."
  p ' ' "quit" "Same as 'exit'."
  p ' ' "back [N=1]" "Enable step mode and try to move back in time N interpreter rounds."
  p ' ' "break POS" "Create a breakpoint at POS."
  p ' ' "breaks" "Display all registered breakpoints."
  p ' ' "cell" "Display the current cell."
  p ' ' "cellat POS" "Display the cell at POS."
  p ' ' "clear POS" "Remove breakpoint at POS."
  p ' ' "clear *" "Remove all breakpoints"
  p ' ' "continue" "Disable step mode."
  p ' ' "dim" "Dispay the funge space dimensions."
  p ' ' "ip" "Dispay the current ip"
  p ' ' "locale XRAD YRAD" "Display the area around the current ip. Only shows cells in the X-Y plane."
  p ' ' "locale RAD" "Same as 'locale RAD RAD'."
  p ' ' "locale" "Same as 'locale XRAD YRAD', where XRAD and YRAD can be set using 'setlocale'."
  p ' ' "nodebug" "Turn off the debugger."
  p ' ' "pop N" "Pops the top N values off the TOSS."
  p ' ' "push VALS" "Push VALS onto the stack. VAL's toward the right are pushed first."
  p ' ' "record N" "Guarantees that 'back' can go back M time steps, provided M <= N and M rounds have passed since 'record N' was invoked."
  p ' ' "setpos POS" "Set the position of the current ip to POS."
  p ' ' "space" "Lists more information that you want to know about the funge space. Warning: This will list every cell and its position."
  p ' ' "s" "Show the TOSS of the current ip."
  p ' ' "setlocale XRAD YRAD" "Defaults the radii displayed by 'locale' to XRAD and YRAD."
  p ' ' "ss" "Show the stack stack of the current ip."
  p ' ' "step" "Enable step mode."
  p ' ' "unshowable CHAR" "Makes the debugger display CHAR for unshowable cells in 'locale'. By default, '®' is used."
  p ' ' "unwatch VALS" "Remove any watchpoints that watch VALS."
  p ' ' "watch VALS" "Creates a watchpoint that watches VALS. Causes the debugger to break when the top of the TOSS is VALS. Left VAL's correspond to top stack values."
  p ' ' "watches" "Display all watch points."

