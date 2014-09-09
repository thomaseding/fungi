module Text.PrintOption (
    printOptionWith
  , printOption
  , showOptionWith
  , showOption
  , PrintSettings (..)
  , defaultPrintSettings
  , fit
  ) where

import Data.List (stripPrefix, groupBy)

-----------------------------------------------------------

data PrintSettings = PrintSettings {
    descriptionColumn :: Int
  , maxColumn :: Int
  , newLinePadding :: String
  , bulletDisplay :: String
  , bulletIndent :: String
  , bulletDelim :: Maybe String
  }

defaultPrintSettings :: PrintSettings
defaultPrintSettings = PrintSettings {
    descriptionColumn = 30
  , maxColumn = 80
  , newLinePadding = replicate 2 ' '
  , bulletDisplay = "- "
  , bulletIndent = "  "
  , bulletDelim = Nothing
  }

afterLast :: (Eq a) => a -> [a] -> [a]
afterLast x xs = afterLast' xs xs
  where
    afterLast' [] zs = zs
    afterLast' (y:ys) zs = if x == y
      then afterLast' ys ys
      else afterLast' ys zs

mapTail :: (a -> a) -> [a] -> [a]
mapTail _ [] = []
mapTail f (x:xs) = x : map f xs

appendLineIfTooLong :: PrintSettings -> String -> String
appendLineIfTooLong settings optionDisplay = if furthestCol < descCol - 1
  then optionDisplay
  else optionDisplay ++ "\n"
  where
    descCol = descriptionColumn settings
    furthestCol = foldr (max . length) 0 $ lines optionDisplay

columnize :: PrintSettings -> Int -> String -> String
columnize settings maxWidth string = columnize' string "" "" False
  where
    padding = newLinePadding settings
    bulDelim = bulletDelim settings
    bulDisp = bulletDisplay settings
    bulIndent = bulletIndent settings
    --
    columnize' :: String -> String -> String -> Bool -> String
    columnize' str currLine' currWord bulleted = case bulDelim >>= (`stripPrefix` str) of
      Just str' -> if overflowsWith currWord
        then currLine <+> currWord <++> columnize' str' bulDisp "" True
        else currLine ++ currWord <++> columnize' str' bulDisp "" True
      Nothing -> case str of
        "" -> if overflowsWith currWord
          then currLine <+> currWord
          else currLine ++ currWord
        ' ':cs -> if overflowsWith currWord
          then currLine <+> columnize' cs "" (currWord ++ " ") bulleted
          else if overflowsWith $ currWord ++ " "
            then currLine ++ currWord <+> columnize' cs "" "" bulleted
            else columnize' cs (currLine ++ currWord ++ " ") "" bulleted
        '\n':cs -> if overflowsWith currWord
          then currLine <+> columnize' ('\n':cs) currWord "" False
          else currLine ++ currWord <++> columnize' cs "" "" False
        c:cs -> columnize' cs currLine (currWord ++ [c]) bulleted
      where
        currLine = dropWhile (== ' ') currLine'
        bulLen = if bulleted
          then length bulDisp
          else 0
        overflowsWith s = length (currLine ++ s) + bulLen > maxWidth
        infixr 5 <++>
        s1 <++> s2 = s1 ++ "\n" ++ padding ++ s2
        infixr 5 <+>
        s1 <+> s2 = if bulleted
          then s1 ++ "\n" ++ padding ++ bulIndent ++ s2
          else s1 <++> s2

constrainDescriptionWidth :: PrintSettings -> String -> String
constrainDescriptionWidth settings = columnize settings (maxCol - descCol)
  where
    maxCol = maxColumn settings
    descCol = descriptionColumn settings

showOptionWith :: PrintSettings -> Char -> String -> String -> String
showOptionWith settings shortOption fullOption description =
  opDisp ++ padding ++ 
    ( id
    . unlines
    . mapTail (replicate (descCol - 1) ' ' ++) 
    . lines
    . constrainDescriptionWidth settings
    $ description)
  where
    descCol = descriptionColumn settings
    hasShort = shortOption /= ' '
    hasFull = fullOption /= ""
    opDisp = appendLineIfTooLong settings $ "" 
      ++ "  "
      ++ [if hasShort then '-' else ' ']
      ++ [shortOption] 
      ++ (if hasFull && hasShort then ", --" else "")
      ++ (if hasFull && not hasShort then "  --" else "")
      ++ fullOption
    paddingLen = descCol - 1 - length (afterLast '\n' opDisp)
    padding = replicate paddingLen ' '

showOption :: Char -> String -> String -> String
showOption = showOptionWith defaultPrintSettings

dot4 :: (e -> f) -> (a -> b -> c -> d -> e) -> (a -> b -> c -> d -> f)
dot4 f g x y z = f . g x y z

printOptionWith :: PrintSettings -> Char -> String -> String -> IO ()
printOptionWith = putStr `dot4` showOptionWith

printOption :: Char -> String -> String -> IO ()
printOption = printOptionWith defaultPrintSettings

fit :: Int -> String -> String
fit maxWidth = fit' 0 . filter (/= " ") . groupBy (\x y -> all (`notElem` [x, y]) " \n")
  where
    fit' :: Int -> [String] -> String
    fit' n strs' = case strs' of 
      [] -> ""
      [str] -> let len = length str
        in case str of
          "\n" -> "\n"
          _ -> if n == 0 && len >= maxWidth
            then str
            else if n + len > maxWidth
              then '\n' : str
              else str
      (str:strs@("\n":_)) -> let len = length str
        in case str of 
          "\n" -> '\n' : fit' 0 strs
          _ -> if n == 0 && len >= maxWidth
            then str ++ fit' 0 strs
            else if n + len > maxWidth
              then '\n' : str ++ " " ++ fit' (len + 1) strs
              else str ++ fit' (n + len) strs
      (str:strs) -> let len = length str
        in case str of 
          "\n" -> '\n' : fit' 0 strs
          _ -> if n == 0 && len >= maxWidth
            then str ++ "\n" ++ fit' 0 strs
            else if n + len + 1 > maxWidth
              then '\n' : str ++ " " ++ fit' (len + 1) strs
              else str ++ " " ++ fit' (n + len + 1) strs

