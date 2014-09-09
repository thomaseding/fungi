module Debug.Debug (
    runDebugger
  ) where

import Control.Monad.State.Strict

import Data.Char (ord)
import qualified Data.Deque as Deque
import qualified Data.History as History
import Data.List (intercalate, genericReplicate, stripPrefix, isPrefixOf, genericTake)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Vector

import Debug.Debugger hiding (runDebugger)

import Space.Cell
import Space.Space

import System.Exit (exitSuccess)
import System.IO (stdin, stdout, hFlush, hGetLine)
import System.IO.Buffering (withBuffering, BufferMode (..))

import Text.Help.Debug (help)
import Text.PrettyShow

import Env
import Instruction
import Ip

-----------------------------------------------------------

updateHistory :: (I i) => Env i -> Env i
updateHistory env = withHistory' (History.push envNoHist) env
  where
    withHistory' = withDebugger . withHistory
    envNoHist = withHistory' (const $ History.empty 0) env

runDebugger :: (I i) => Instruction i ()
runDebugger = runDebugger' >> modify updateHistory

runDebugger' :: (I i) => Instruction i ()
runDebugger' = do
  env <- get
  let dim = getDim env
      toss = currentToss env
      debugger = getDebugger env
      breakPoints = getBreakPoints debugger
      watchExprs = getWatchExprs debugger
      currPos = getPos $ currentIp env
  case getDebugMode debugger of
    DebugOff -> return ()
    DebugContinue
      | genericTake dim (unVector currPos) `Set.member` breakPoints -> stepDebug >> go
      | any (`isPrefixOf` Deque.toList toss) $ Set.toList watchExprs -> stepDebug >> go
      | otherwise -> return ()
    DebugStep -> go
  where
    go :: (I i) => Instruction i ()
    go = do
      liftIO $ putChar '\n'
      cellDebug
      ipDebug
      sDebug
      gets (getLocaleRads . getDebugger) >>= localeDebug . Just . mapPair fromIntegral
      fetchDebugCommand

fetchDebugCommand :: (I i) => Instruction i ()
fetchDebugCommand = do
  cmd <- liftIO $ do
    putStr "> "
    hFlush stdout
    withBuffering LineBuffering stdin hGetLine
  case delimWords "'\"" cmd of
    Nothing -> do
      liftIO $ putStrLn $ "Unknown command: " ++ cmd
      fetchDebugCommand
    Just cmd' -> case mapHead (dropWhile (== '-')) cmd' of
      [] -> return ()
      "?" : []
        -> helpDebug >> fetchDebugCommand
      "back" : []
        -> backDebug "1"
      "back" : arg : []
        -> backDebug arg
      "break" : args
        -> breakDebug args >> fetchDebugCommand
      "breaks" : []
        -> breaksDebug >> fetchDebugCommand
      "cell" : []
        -> cellDebug >> fetchDebugCommand
      "cellat" : args
        -> cellatDebug args >> fetchDebugCommand
      "clear" : args
        -> clearDebug args >> fetchDebugCommand
      "continue" : []
        -> continueDebug
      "dim" : []
        -> dimDebug >> fetchDebugCommand
      "exit" : []
        -> quitDebug
      "help" : []
        -> helpDebug >> fetchDebugCommand
      "ip" : []
        -> ipDebug >> fetchDebugCommand
      "locale" : []
        -> localeDebug Nothing >> fetchDebugCommand
      "locale" : rad : []
        -> localeDebug (fmap duplicate $ tryRead rad) >> fetchDebugCommand
      "locale" : xrad : yrad : []
        -> localeDebug (liftM2 (,) (tryRead xrad) (tryRead yrad)) >> fetchDebugCommand
      "nodebug" : []
        -> nodebugDebug
      "pop" : arg : []
        -> popDebug arg >> fetchDebugCommand
      "push" : args
        -> pushDebug args >> fetchDebugCommand
      "quit" : []
        -> quitDebug
      "record" : arg : []
        -> recordDebug arg >> fetchDebugCommand
      "setpos" : args
        -> setposDebug args >> fetchDebugCommand
      "space" : []
        -> spaceDebug >> fetchDebugCommand
      "s" : []
        -> sDebug >> fetchDebugCommand
      "setlocale" : xrad : yrad : []
        -> setlocaleDebug xrad yrad >> fetchDebugCommand
      "ss" : []
        -> ssDebug >> fetchDebugCommand
      "step" : []
        -> stepDebug >> fetchDebugCommand
      "unshowable" : arg : []
        -> unshowableDebug arg >> fetchDebugCommand
      "unwatch" : args
        -> unwatchDebug args >> fetchDebugCommand
      "watch" : args
        -> watchDebug args >> fetchDebugCommand
      "watches" : []
        -> watchesDebug >> fetchDebugCommand
      _ 
        -> liftIO (putStrLn $ "Unknown command: " ++ cmd) >> fetchDebugCommand

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)

duplicate :: a -> (a, a)
duplicate x = (x, x)

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ [] = []
mapHead f (x:xs) = f x : xs

delimWords :: String -> String -> Maybe [String]
delimWords delims str = fmap (reverse . dropWhile (== "") . reverse) $ delimWords' delims str

delimWords' :: String -> String -> Maybe [String]
delimWords' _ "" = Just [""]
delimWords' delims (c:cs)
  | c == ' ' = fmap ("" :) $ delimWords' delims $ dropWhile (== ' ') cs
  | c `elem` delims = fmap (mapHead (\str -> c : takeWhile (/= c) cs ++ str ++ [c])) $ delimWords'' c cs
  | otherwise = fmap (mapHead (c:)) $ delimWords' delims cs
  where
    delimWords'' currDelim = delimWords' delims <=< stripPrefix [currDelim] . dropWhile (/= currDelim)

-----------------------------------------------------------

readVector :: (I i) => [String] -> Maybe (Vector i)
readVector [] = Nothing
readVector strs = case tryReadList strs of
  Nothing -> Nothing
  Just ns -> Just $ mkVector $ ns ++ repeat 0

tryRead :: (Read a) => String -> Maybe a
tryRead str = case reads str of
  [(x, "")] -> Just x
  _ -> Nothing

tryReadList :: (Read a) => [String] -> Maybe [a]
tryReadList [] = Just []
tryReadList (str:strs) = tryRead str >>= \x -> fmap (x :) $ tryReadList strs

readCells :: (I i) => [String] -> [Maybe i]
readCells [] = []
readCells (str:strs) = case str of
  '\'' : str' -> readString '\'' str'
  '"'  : str' -> readString '"'  str'
  "" -> readCells strs
  _ -> case tryRead str of
    Just n -> Just n : readCells strs
    Nothing -> case str of
      [c] -> (Just . fromIntegral . ord) c : readCells strs
      _ -> Nothing : readCells strs
  where
    readString quote s = case sequence (readString' quote s) of
      Nothing -> Nothing : readCells strs
      Just cells -> cells ++ readCells strs
    --
    readString' _ "" = [Nothing]
    readString' quote (c:cs)
      | c == quote = case cs of
        "" -> []
        _ -> [Nothing]
      | otherwise = (Just . Just . fromIntegral . ord) c : readString' quote cs
    
-----------------------------------------------------------

invalidVector :: Instruction i ()
invalidVector = liftIO $ putStrLn "Invalid vector."

invalidArgument :: Instruction i ()
invalidArgument = liftIO $ putStrLn "Invalid argument."

-----------------------------------------------------------

backDebug :: (I i) => String -> Instruction i ()
backDebug arg = case tryRead arg of 
  Nothing -> invalidArgument
  Just n -> if n <= 0
    then invalidArgument >> fetchDebugCommand
    else do
      hist <- gets $ getHistory . getDebugger
      case History.lookup (n - 1) hist of
        Nothing -> do
          liftIO $ putStrLn "Not enough history. Try again."
          fetchDebugCommand
        Just env' -> do
          let hist' = History.popN n hist
          put env'
          modify $ withDebugger $ \d -> d { getDebugMode = DebugStep, getHistory = hist' }
          runDebugger'

breakDebug :: (I i) => [String] -> Instruction i ()
breakDebug args = case readVector args of
  Nothing -> invalidVector
  Just pos -> modify $ addBreakPoint $ unVector pos

breaksDebug :: (I i) => Instruction i ()
breaksDebug = do
  breakPoints <- gets $ Set.toList . getBreakPoints . getDebugger
  liftIO $ mapM_ pprint breakPoints

cellDebug :: (I i) => Instruction i ()
cellDebug = gets currentCell >>= liftIO . pprint

cellatDebug :: (I i) => [String] -> Instruction i ()
cellatDebug args = case readVector args of
  Nothing -> invalidVector
  Just pos -> do
    dim <- gets getDim
    gets getSpace >>= liftIO . pprint . flip cellAt (takeV dim $ pos `append` 0)

clearDebug :: (I i) => [String] -> Instruction i ()
clearDebug args
  | args == ["*"] = modify $ withDebugger $ withBreakPoints $ const Set.empty
  | otherwise = case readVector args of
      Nothing -> invalidVector
      Just pos -> modify $ removeBreakPoint $ unVector pos

continueDebug :: (I i) => Instruction i ()
continueDebug = modify $ withDebugger $ \d -> d { getDebugMode = DebugContinue }

dimDebug :: (I i) => Instruction i ()
dimDebug = gets getDim >>= liftIO . pprint 

helpDebug :: Instruction i ()
helpDebug = liftIO help

ipDebug :: (I i) => Instruction i ()
ipDebug = gets currentIp >>= liftIO . pprint

localeDebug :: (I i) => Maybe (Int, Int) -> Instruction i ()
localeDebug mrads = do
  env <- get
  let unshowableCellChar = getUnshowableChar env
      dim = getDim env
      s = getSpace env
      pos = getPos . currentIp $ env
      defaultRads = getLocaleRads $ getDebugger env
      (xrad, yrad) = mapPair (fromIntegral . min 666 . max 1) $ fromMaybe defaultRads mrads
      top = "+" ++ rep xrad '-' ++ "v" ++ rep xrad '-' ++ "+"
      bot = "+" ++ rep xrad '-' ++ "^" ++ rep xrad '-' ++ "+"
      topRows = intercalate "\n" ["|" ++ chars2 [[x, y] | x <- [-xrad .. xrad]] ++ "|" | y <- [-yrad .. -1]]
      midRow  = ">" ++ chars1 [[x, 0] | x <- [-xrad .. xrad]] ++ "<"
      botRows = intercalate "\n" ["|" ++ chars2 [[x, y] | x <- [-xrad .. xrad]] ++ "|" | y <- [1 .. yrad]]
      str = unlines [top, topRows, midRow, botRows, bot]
      disp xs = takeV dim $ pos + mkVector xs
      chars1 = map (showCell . cellAt s . disp)
      chars2 = if dim >= 2
        then chars1
        else const . rep (2 * xrad + 1) $ ' '
      showCell = fromMaybe unshowableCellChar . cellToPrintableChar
  liftIO $ putStrLn str
  where
    rep = genericReplicate

nodebugDebug :: (I i) => Instruction i ()
nodebugDebug = modify $ withDebugger $ \d -> d { getDebugMode = DebugOff }

popDebug :: (I i) => String -> Instruction i ()
popDebug arg = case tryRead arg :: Maybe Integer of
  Nothing -> invalidArgument
  Just n -> popNInstr_ n

pushDebug :: (I i) => [String] -> Instruction i ()
pushDebug args = case sequence $ readCells args of
  Nothing -> invalidArgument
  Just cells -> pushVectorInstr $ mkVector $ reverse cells

quitDebug :: (I i) => Instruction i ()
quitDebug = liftIO exitSuccess

recordDebug :: (I i) => String -> Instruction i ()
recordDebug arg = case tryRead arg of
  Nothing -> invalidArgument
  Just n -> modify $ withDebugger $ withHistory $ History.fromHistory $ Just n

setposDebug :: (I i) => [String] -> Instruction i ()
setposDebug args = case readVector args of
  Nothing -> invalidVector
  Just pos -> setPosInstr pos

spaceDebug :: (I i) => Instruction i ()
spaceDebug = gets getSpace >>= liftIO . pprint

sDebug :: (I i) => Instruction i ()
sDebug = do
  toss <- gets currentToss
  let ptInt = pshow toss
      ptGuts = id 
        . words 
        . map (\c -> if c == ',' then ' ' else c) 
        . reverse 
        . drop 3 
        . reverse 
        . drop 1 
        $ ptInt
      ptChar = "[" ++ intercalate "," (map (showChr . read) ptGuts) ++ "..]"
  liftIO $ putStrLn ptChar >> putStrLn ptInt
  where
    showChr :: Integer -> String
    showChr n = maybe "\\???" show $ cellToChar $ chrCell n

setlocaleDebug :: (I i) => String -> String -> Instruction i ()
setlocaleDebug xrad yrad = case tryRead xrad of
  Nothing -> invalidArgument
  Just xrad' -> case tryRead yrad of
    Nothing -> invalidArgument
    Just yrad' -> modify $ withDebugger $ \d -> d {
        getLocaleRads = (xrad', yrad')
      }

ssDebug :: (I i) => Instruction i ()
ssDebug = gets currentSs >>= liftIO . pprint 

stepDebug :: (I i) => Instruction i ()
stepDebug = modify $ withDebugger $ \d -> d { getDebugMode = DebugStep }

unshowableDebug :: (I i) => String -> Instruction i ()
unshowableDebug str = case str of 
  [c] -> modify $ \env -> env { getUnshowableChar = c }
  _ -> invalidArgument

unwatchDebug :: (I i) => [String] -> Instruction i ()
unwatchDebug args = case sequence $ readCells args of 
  Nothing -> invalidArgument
  Just cells -> modify $ withDebugger $ withWatchExprs $ Set.delete cells

watchDebug :: (I i) => [String] -> Instruction i ()
watchDebug args = case sequence $ readCells args of 
  Nothing -> invalidArgument
  Just cells -> modify $ withDebugger $ withWatchExprs $ Set.insert cells

watchesDebug :: (I i) => Instruction i ()
watchesDebug = gets (Set.toList . getWatchExprs . getDebugger) >>= liftIO . mapM_ pprint

