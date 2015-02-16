module Instruction (
    I
  , Instruction
  , buildInstructions
  , baseInstructions
  , lookupInstruction
  , runCurrentInstruction
  , currentInstruction

  , guardDim
  , guardZero

  , ifInstr
  , moveByDeltaInstr
  , setDeltaInstr
  , setPosInstr
  , opInstr
  , op2Instr
  , pushVectorInstr
  , popInstr
  , popNInstr_
  , popVectorInstr
  , popDimVectorInstr
  , popStringInstr
  , popStringInstr'
  , tryLiftIO

  , nopInstr
  , pushInstr
  , trampolineInstr
  , reverseInstr
  , loadSemanticsInstr
  , unloadSemanticsInstr
  , beginBlockInstr
  , endBlockInstr
  , turnLeftInstr
  , turnRightInstr
  , putInstr
  , getInstr
  , logicalNotInstr
  , goWestInstr
  , goEastInstr
  , goNorthInstr
  , goSouthInstr
  , goLowInstr
  , goHighInstr
  , northSouthIfInstr
  , eastWestIfInstr
  , stopInstr
  , quitInstr
  , subtractInstr
  , addInstr
  , multiplyInstr
  , divideInstr
  , outputFileInstr
  , inputFileInstr
  , outputCharacterInstr
  , inputCharacterInstr
  , outputDecimalInstr
  , inputDecimalInstr
  , stringModeInstr
  , fetchCharacterInstr
  , unknownInstr
  , spaceInstr
  , remainderInstr
  , greaterThanInstr
  , goAwayInstr
  , duplicateInstr
  , swapInstr
  , popInstr_

  ) where

import Control.Exception (catch)
import Control.Monad.State.Strict

import Data.ByteSize
import qualified Data.ByteString.Char8 as BS (hGetContents, unpack)
import Data.Char (ord)
import Data.Deque
import qualified Data.Deque as Deque
import Data.I
import Data.IntegralLike
import Data.List (foldl', genericLength, genericReplicate, intercalate)
import Data.LogicalBits
import Data.MaybeBounded
import Data.Maybe (isNothing, fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Stack as Stack
import Data.Tuple.Map
import Data.Vector

import Debug.Debugger

import Space.Cell
import Space.Space

import System.Cmd (system)
import System.Directory (canonicalizePath)
import System.Environment (getEnvironment)
import System.Exit (exitWith, exitFailure, ExitCode (..))
import System.FilePath (pathSeparator, takeDirectory, isAbsolute)
import System.IO (IOMode (..), withFile, hPutStr, hFlush, hGetChar, stdout, stdin)
import System.IO.Buffering (BufferMode (..), withBuffering)
import System.Random (randomRIO)
import System.Time (CalendarTime (..), getClockTime, toCalendarTime)

import Text.PrettyShow

import Env
import Ip
import qualified Mode
import qualified Semantics
import UnknownInstruction
import Version

-----------------------------------------------------------

infixr 9 `o`
o :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
o = (.).(.)

numToBool :: (Eq a, Num a) => a -> Bool
numToBool 0 = False
numToBool _ = True

whileM :: (Monad m) => (a -> m Bool) -> (a -> m a) -> a -> m a
whileM mPred f x = mPred x >>= \bool -> if bool
  then f x >>= whileM mPred f
  else return x

whileM_ :: (Monad m) => m Bool -> m () -> m ()
whileM_ mBool m = whileM (const mBool) (const m) ()

genericReplicateM :: (Integral i, Monad m) => i -> m a -> m [a]
genericReplicateM = sequence `o` genericReplicate

genericReplicateM_ :: (Integral i, Monad m) => i -> m a -> m ()
genericReplicateM_ = sequence_ `o` genericReplicate

-----------------------------------------------------------

type Instruction i = StateT (Env i) IO

-----------------------------------------------------------

buildInstructions :: (Integral i, IntegralLike c) => [(c, v)] -> Map i v
buildInstructions = Map.fromList . map (map1 asIntegral)

baseInstructions :: (I i) => Map i (Instruction i ())
baseInstructions = buildInstructions $ [
    (' ', spaceInstr)
  , ('!', logicalNotInstr)
  , ('"', stringModeInstr)
  , ('#', trampolineInstr)
  , ('$', popInstr_)
  , ('%', remainderInstr)
  , ('&', inputDecimalInstr)
  , ('\'',fetchCharacterInstr)
  , ('(', loadSemanticsInstr)
  , (')', unloadSemanticsInstr)
  , ('*', multiplyInstr)
  , ('+', addInstr)
  , (',', outputCharacterInstr)
  , ('-', subtractInstr)
  , ('.', outputDecimalInstr)
  , ('/', divideInstr)
  , ('0', pushInstr 0)
  , ('1', pushInstr 1)
  , ('2', pushInstr 2)
  , ('3', pushInstr 3)
  , ('4', pushInstr 4)
  , ('5', pushInstr 5)
  , ('6', pushInstr 6)
  , ('7', pushInstr 7)
  , ('8', pushInstr 8)
  , ('9', pushInstr 9)
  , (':', duplicateInstr)
  , (';', jumpOverInstr)
  , ('<', goWestInstr)
  , ('=', executeInstr)
  , ('>', goEastInstr)
  , ('?', goAwayInstr)
  , ('@', stopInstr)
  , ('[', turnLeftInstr)
  , ('\\',swapInstr)
  , (']', turnRightInstr)
  , ('^', goNorthInstr)
  , ('_', eastWestIfInstr)
  , ('`', greaterThanInstr)
  , ('a', pushInstr 10)
  , ('b', pushInstr 11)
  , ('c', pushInstr 12)
  , ('d', pushInstr 13)
  , ('e', pushInstr 14)
  , ('f', pushInstr 15)
  , ('g', getInstr)
  , ('h', goHighInstr)
  , ('i', inputFileInstr)
  , ('j', jumpForwardInstr)
  , ('k', iterateInstr)
  , ('l', goLowInstr)
  , ('m', highLowIfInstr)
  , ('n', clearStackInstr)
  , ('o', outputFileInstr)
  , ('p', putInstr)
  , ('q', quitInstr)
  , ('r', reverseInstr)
  , ('s', storeCharacterInstr)
  , ('t', splitInstr)
  , ('u', stackUnderStackInstr)
  , ('v', goSouthInstr)
  , ('w', compareInstr)
  , ('x', absoluteDeltaInstr)
  , ('y', getSysInfoInstr)
  , ('z', nopInstr)
  , ('{', beginBlockInstr)
  , ('|', northSouthIfInstr)
  , ('}', endBlockInstr)
  , ('~', inputCharacterInstr)
  ] ++ zip ['A'..'Z'] (repeat reverseInstr)

lookupInstruction :: (I i) => i -> Instruction i ()
lookupInstruction i = do
  sem <- gets $ getSemantics . currentIp
  fromMaybe (unknownInstr i) $ Semantics.lookup i sem

unknownInstr :: (I i) => i -> Instruction i ()
unknownInstr i = do
  env <- get
  case getUnknownMode env of
    ReverseUnknown -> reverseInstr
    FailUnknown -> liftIO $ do
      printUnknown
      exitFailure
    DebugUnknown -> do
      liftIO $ do
        printUnknown
        pprint $ getPos $ currentIp env
      debug
      reverseInstr
  where
    printUnknown = putStrLn $ "\n\n*** Uknown instruction ord: " ++ pshow i

-----------------------------------------------------------

currentInstruction :: (I i) => Env i -> Instruction i ()
currentInstruction = lookupInstruction . ordCell . currentCell

runCurrentInstruction :: (I i) => Instruction i ()
runCurrentInstruction = do
  env <- get
  runDebugger (getDebugger env)
  env' <- get
  currentInstruction env'

debug :: Instruction i ()
debug = do
  modify $ withDebugger $ \d -> d { getDebugMode = DebugStep }
  gets getDebugger >>= runDebugger

-----------------------------------------------------------

popInstr_ :: (I i) => Instruction i ()
popInstr_ = popInstr >> return ()

pushVectorInstr :: (I i) => Vector i -> Instruction i ()
pushVectorInstr = mapM_ pushInstr . unVector

popVectorInstr :: (I i, Integral n) => n -> Instruction i (Vector i)
popVectorInstr = liftM mkVector . popNInstr

popNInstr :: (I i, Integral n) => n -> Instruction i [i]
popNInstr n = liftM reverse $ genericReplicateM n popInstr

popNInstr_ :: (I i, Integral n) => n -> Instruction i ()
popNInstr_ n = genericReplicateM_ n popInstr

popDimInstr :: (I i) => Instruction i [i]
popDimInstr = gets getDim >>= popNInstr

popDimVectorInstr :: (I i) => Instruction i (Vector i)
popDimVectorInstr = gets getDim >>= popVectorInstr

popStringInstr :: (I i) => Instruction i [Maybe Char]
popStringInstr = liftM (map $ cellToChar . chrCell) popStringInstr'

popStringInstr' :: (I i) => Instruction i [i]
popStringInstr' = do
  x <- popInstr
  if x == 0
    then return []
    else liftM (x :) popStringInstr'

guardDim :: (I i) => Int -> Instruction i () -> Instruction i ()
guardDim dim instr = do
  env <- get
  if getDim env < dim
    then reverseInstr
    else instr

opInstr :: (I i) => (i -> i) -> Instruction i ()
opInstr op = popInstr >>= pushInstr . op

op2Instr :: (I i) => (i -> i -> i) -> Instruction i ()
op2Instr op = do
  y <- popInstr 
  x <- popInstr
  pushInstr $ op x y

-----------------------------------------------------------

nopInstr :: (I i) => Instruction i ()
nopInstr = return ()

-----------------------------------------------------------
-- Direction Changing
-----------------------------------------------------------

setDeltaInstr :: (I i) => [i] -> Instruction i ()
setDeltaInstr delta = do
  dim <- gets getDim
  let delta' = take dim $ delta ++ repeat 0
  modify $ withIp $ setDelta $ mkVector delta'

goEastInstr :: (I i) => Instruction i ()
goEastInstr = setDeltaInstr [1]

goWestInstr :: (I i) => Instruction i ()
goWestInstr = setDeltaInstr [-1]

goNorthInstr :: (I i) => Instruction i ()
goNorthInstr = guardDim 2 $ setDeltaInstr [0, -1]

goSouthInstr :: (I i) => Instruction i ()
goSouthInstr = guardDim 2 $ setDeltaInstr [0, 1]

goHighInstr :: (I i) => Instruction i ()
goHighInstr = guardDim 3 $ setDeltaInstr [0, 0, 1]

goLowInstr :: (I i) => Instruction i ()
goLowInstr = guardDim 3 $ setDeltaInstr [0, 0, -1]

randElem :: [a] -> IO a
randElem xs = (xs !!) `fmap` randomRIO (0, length xs - 1)

goAwayInstr :: (I i) => Instruction i ()
goAwayInstr = do
  dim <- gets getDim
  delta <- liftIO $ do
    dir <- randElem [-1, 1]
    axis <- randElem [0 .. dim - 1]
    return $ genericReplicate axis 0 ++ [dir]
  setDeltaInstr delta

turnRightInstr :: (I i) => Instruction i ()
turnRightInstr = guardDim 2 $ modify $ withIp turn
  where
    turn ip = setDelta (mkVector $ [-y, x] ++ zs) ip
      where
        ([x, y], zs) = splitAt 2 . unVector . getDelta $ ip

turnLeftInstr :: (I i) => Instruction i ()
turnLeftInstr = replicateM_ 3 turnRightInstr

reverseInstr :: (I i) => Instruction i ()
reverseInstr = modify $ withIp reverseIp

absoluteDeltaInstr :: (I i) => Instruction i ()
absoluteDeltaInstr = popDimInstr >>= setDeltaInstr

-----------------------------------------------------------
-- Flow Control
-----------------------------------------------------------

setPosInstr :: (I i) => Vector i -> Instruction i ()
setPosInstr = modify . withIp . setPos

trampolineInstr :: (I i) => Instruction i ()
trampolineInstr = do
  env <- get
  let s = getSpace env
      ip = currentIp env
      delta = getDelta ip
      pos = getPos ip
  modify $ withIp $ setPos $ (pos `travelBy` delta) s

trampolineBackInstr :: (I i) => Instruction i ()
trampolineBackInstr = reverseInstr >> trampolineInstr >> reverseInstr

moveByDeltaInstr :: (I i) => Instruction i ()
moveByDeltaInstr = modify $ withIp $ \ip -> let
  pos = getPos ip
  delta = getDelta ip
  in ip { getPos = pos + delta }

moveBackByDeltaInstr :: (I i) => Instruction i ()
moveBackByDeltaInstr = reverseInstr >> moveByDeltaInstr >> reverseInstr

stopInstr :: (I i) => Instruction i ()
stopInstr = modify $ \env -> let
  ip = currentIp env
  ident = getId ip
  env' = env { getValidIds = ident : getValidIds env }
  in killIp `withIp` env'

atInstr :: (I i) => Char -> Instruction i Bool
atInstr c = gets $ (charToCell c ==) . currentCell

spaceInstr :: (I i) => Instruction i ()
spaceInstr = whileM_ (atInstr ' ') trampolineInstr >> runCurrentInstruction

jumpOverInstr :: (I i) => Instruction i ()
jumpOverInstr = trampolineInstr >> jump >> trampolineInstr >> runCurrentInstruction
  where
    jump = whileM_ (liftM not $ atInstr ';') trampolineInstr

jumpForwardInstr :: (I i) => Instruction i ()
jumpForwardInstr = do
  n <- popInstr
  genericReplicateM_ (abs n) $ if n >= 0
    then trampolineInstr
    else trampolineBackInstr

quitInstr :: (I i) => Instruction i ()
quitInstr = do
  exitVal <- popInstr
  liftIO $ if exitVal == 0
    then exitWith ExitSuccess
    else let
      maxInt = fromIntegral (maxBound :: Int)
      exitVal' = fromIntegral $ min maxInt exitVal
      in exitWith $ ExitFailure exitVal'

anyM :: (Monad m) => [m Bool] -> m Bool
anyM = foldr (liftM2 (||)) $ return False

iterateInstr :: (I i) => Instruction i ()
iterateInstr = do
  initPos <- gets $ getPos . currentIp
  n <- popInstr
  trampolineInstr
  whileM_ (anyM $ map atInstr " ;") trampolineInstr
  instr <- gets currentInstruction
  when (n > 0) $ do
    setPosInstr initPos
    genericReplicateM_ n instr

-----------------------------------------------------------
-- Decision Making
-----------------------------------------------------------

logicalNotInstr :: (I i) => Instruction i ()
logicalNotInstr = opInstr (asIntegral . not . numToBool)

greaterThanInstr :: (I i) => Instruction i ()
greaterThanInstr = op2Instr (asIntegral `o` (>))

ifInstr :: (I i) => Instruction i a -> Instruction i a -> Instruction i a
ifInstr trueInstr falseInstr = popInstr >>= \n -> if n == 0
  then falseInstr
  else trueInstr

eastWestIfInstr :: (I i) => Instruction i ()
eastWestIfInstr = ifInstr goWestInstr goEastInstr

northSouthIfInstr :: (I i) => Instruction i ()
northSouthIfInstr = guardDim 2 $ ifInstr goNorthInstr goSouthInstr

highLowIfInstr :: (I i) => Instruction i ()
highLowIfInstr = guardDim 3 $ ifInstr goHighInstr goLowInstr

compareInstr :: (I i) => Instruction i ()
compareInstr = guardDim 2 $ do
  y <- popInstr
  x <- popInstr
  case compare x y of
    LT -> turnLeftInstr
    GT -> turnRightInstr
    EQ -> nopInstr

-----------------------------------------------------------
-- Integers
-----------------------------------------------------------

addInstr :: (I i) => Instruction i ()
addInstr = op2Instr (+)

subtractInstr :: (I i) => Instruction i ()
subtractInstr = op2Instr (-)

multiplyInstr :: (I i) => Instruction i ()
multiplyInstr = op2Instr (*)

guardZero :: (Eq a, Num a) => (a -> a -> a) -> (a -> a -> a)
guardZero f x y = if y == 0
  then 0
  else f x y

divideInstr :: (I i) => Instruction i ()
divideInstr = op2Instr $ guardZero div

remainderInstr :: (I i) => Instruction i ()
remainderInstr = op2Instr $ guardZero rem

-----------------------------------------------------------
-- Strings
-----------------------------------------------------------

stringModeInstructions :: (I i) => (i -> Maybe (Instruction i ()), Map i (Maybe (Instruction i ())))
stringModeInstructions = (,) (Just . pushInstr) $ buildInstructions [
    ('"', Just stringModeInstr)
  , (' ', Just sgmlSpaceInstr)
  ]
  where
    spaceOrd = ordCell $ charToCell ' '
    sgmlSpaceInstr = do
      pushInstr spaceOrd
      whileM_ (atInstr ' ') trampolineInstr
      moveBackByDeltaInstr

stringModeInstr :: (I i) => Instruction i ()
stringModeInstr = do
  modify $ withIp $ toggleMode mode
  modify $ withSemantics $ Semantics.toggleOverlay mode stringModeInstructions
  where
    mode = Mode.String

fetchCharacterInstr :: (I i) => Instruction i ()
fetchCharacterInstr = do
  trampolineInstr
  x <- gets $ ordCell . currentCell
  pushInstr x

storeCharacterInstr :: (I i) => Instruction i ()
storeCharacterInstr = do
  trampolineInstr
  pos <- gets $ getPos . currentIp
  pushVectorInstr pos
  putInstr

-----------------------------------------------------------
-- Stack Manipulation
-----------------------------------------------------------

popInstr :: (I i) => Instruction i i
popInstr = do
  qm <- gets $ testMode Mode.Queue . currentIp
  liftM (fromMaybe 0) $ if qm
    then do
      x <- gets $ bottom . currentToss
      modify $ withToss popBottom
      return x
    else do
      x <- gets $ Deque.top . currentToss
      modify $ withToss Deque.pop
      return x

pushInstr :: (I i) => i -> Instruction i ()
pushInstr n = do
  im <- gets $ testMode Mode.Invert . currentIp
  modify $ withToss $ if im
    then pushBottom n
    else Deque.push n

duplicateInstr :: (I i) => Instruction i ()
duplicateInstr = popInstr >>= replicateM_ 2 . pushInstr

swapInstr :: (I i) => Instruction i ()
swapInstr = replicateM 2 popInstr >>= mapM_ pushInstr

clearStackInstr :: (I i) => Instruction i ()
clearStackInstr = modify $ withToss $ const mkDeque

-----------------------------------------------------------
-- Stack Stack Manipulation
-----------------------------------------------------------

beginBlockInstr :: (I i) => Instruction i ()
beginBlockInstr = do
  n <- popInstr
  elts <- popVectorInstr n
  ip <- gets currentIp
  pushVectorInstr $ mkVector $ genericReplicate (negate n) 0
  pushVectorInstr $ getStorageOffset ip
  let so = getPos ip + getDelta ip
  modify $ \env -> setStorageOffset so `withIp` (Stack.push mkDeque `withSs` env)
  pushVectorInstr elts

guardSoss :: (I i) => Instruction i () -> Instruction i ()
guardSoss instr = do
  noSoss <- gets $ Stack.isEmpty . Stack.pop . currentSs
  if noSoss
    then reverseInstr
    else instr

endBlockInstr :: (I i) => Instruction i ()
endBlockInstr = guardSoss $ do
  n <- popInstr
  elts <- popVectorInstr n
  modify $ withSs Stack.pop
  popDimVectorInstr >>= modify . withIp . setStorageOffset
  if n >= 0
    then pushVectorInstr elts
    else popNInstr_ $ abs n

stackUnderStackInstr :: (I i) => Instruction i ()
stackUnderStackInstr = guardSoss $ do
  n <- popInstr
  case compare n 0 of
    GT -> do
      toss <- gets currentToss
      modify $ withSs Stack.pop
      elts <- popVectorInstr n
      modify $ withSs $ Stack.push toss
      pushVectorInstr $ reverseV elts
    LT -> do
      elts <- popVectorInstr $ abs n
      toss <- gets currentToss
      modify $ withSs Stack.pop
      pushVectorInstr $ reverseV elts
      modify $ withSs $ Stack.push toss
    EQ -> nopInstr

-----------------------------------------------------------
-- Funge-Space Storage
-----------------------------------------------------------

putInstr :: (I i) => Instruction i ()
putInstr = do
  loc <- popDimVectorInstr
  x <- popInstr
  offset <- gets $ getStorageOffset . currentIp
  modify $ withSpace $ \s -> putCell s (chrCell x) $ loc + offset

getInstr :: (I i) => Instruction i ()
getInstr = do
  loc <- popDimVectorInstr
  env <- get
  let s = getSpace env
      offset = getStorageOffset $ currentIp env
      x = ordCell $ cellAt s $ loc + offset
  pushInstr x

-----------------------------------------------------------
-- Standard Input/Output
-----------------------------------------------------------

tryLiftIO :: IO a -> Instruction i (Maybe a)
tryLiftIO io = liftIO $ catch (liftM Just io) $ \e -> const (return Nothing) (e :: IOError)

isDigit :: Char -> Bool
isDigit = (`elem` ['0'..'9'])

outputDecimalInstr :: (I i) => Instruction i ()
outputDecimalInstr = do
  x <- popInstr
  outcome <- tryLiftIO $ putStr $ show x ++ " "
  when (isNothing outcome) reverseInstr

outputCharacterInstr :: (I i) => Instruction i ()
outputCharacterInstr = do
  c <- liftM (fromMaybe '?' . cellToChar . chrCell) popInstr
  outcome <- tryLiftIO $ putChar c
  when (isNothing outcome) reverseInstr

inputCharacterInstr :: (I i) => Instruction i ()
inputCharacterInstr = do
  outcome <- tryLiftIO $ do
    hFlush stdout
    withBuffering NoBuffering stdin hGetChar
  case outcome of
    Nothing -> reverseInstr
    Just c -> let
      n = fromIntegral $ ord c
      in pushInstr n

inputDecimalInstr :: (I i) => Instruction i ()
inputDecimalInstr = do
  ident <- gets $ getId . currentIp
  outcome <- tryLiftIO $ do
    hFlush stdout
    withBuffering NoBuffering stdin $ const $ getDecimal ident
  maybe reverseInstr pushInstr outcome

getDecimal :: (I i) => i -> IO i
getDecimal iType = do
  c <- getChar
  let k = read [c] :: Integer
      k' = fromInteger k
  if isDigit c
    then case maybeMaxBound `asTypeOf` Just iType of
      Just bound -> if k > fromIntegral bound
        then getDecimal iType
        else getDecimal' k'
      Nothing -> getDecimal' k'
    else getDecimal iType

getDecimal' :: (I i) => i -> IO i
getDecimal' n = do
  c <- getChar
  if isDigit c
    then let
      k = read [c]
      n' = 10 * (fromIntegral n :: Integer) + k
      n'' = fromInteger n'
      in case maybeMaxBound `asTypeOf` Just n of
        Just bound -> if n' > fromIntegral bound
          then return n
          else getDecimal' n''
        Nothing -> getDecimal' n''
    else return n
    
-----------------------------------------------------------
-- File Input/Output
-----------------------------------------------------------

canonicalizePath' :: (I i) => FilePath -> Instruction i FilePath
canonicalizePath' path = if isAbsolute path
  then return path
  else do
    progName <- gets getProgName
    dir <- liftIO $ liftM takeDirectory $ canonicalizePath progName
    return $ dir ++ [pathSeparator] ++ path

inputFileInstr :: (I i) => Instruction i ()
inputFileInstr = do
  env <- get
  let space = getSpace env
      so = getStorageOffset $ currentIp env
      dim = getDim env
      east = takeV dim $ 1 `cons` 0
  mFilepath <- liftM sequence popStringInstr
  flag <- popInstr
  va <- liftM (so +) popDimVectorInstr
  case mFilepath of
    Nothing -> reverseInstr
    Just filepath -> do
      cfilepath <- canonicalizePath' filepath
      m_space_vb <- tryLiftIO $ if testLogicalBit flag 0
        then do
          cells <- withFile cfilepath ReadMode $ \handle -> liftM (map charToCell . BS.unpack) $ BS.hGetContents handle
          let space' = foldl' (\s (c, p) -> putCell s c p) space $ zip cells $ iterate (+ east) va
              vb = (takeV dim $ genericLength cells `cons` 0)
          return (space', vb)
        else do
          kidSpace <- withFile cfilepath ReadMode $ \handle -> do
            contents <- BS.hGetContents handle
            return $ mkSpace dim contents
          let (minPos, maxPos, _) = minMaxCoords kidSpace
              vb = maxPos - minPos + takeV dim 1
              space' = putSpaceAt va space kidSpace
          return (space', vb)
      case m_space_vb of
        Nothing -> reverseInstr
        Just (space', vb) -> do
          pushVectorInstr vb
          pushVectorInstr va
          modify $ withSpace $ const space'

linearize :: String -> String
linearize = unlines . map (reverse . dropWhile (== ' ') . reverse) . lines

outputFileInstr :: (I i) => Instruction i ()
outputFileInstr = do
  env <- get
  let dim = getDim env
      space = getSpace env
      so = getStorageOffset $ currentIp env
  mFilepath <- liftM sequence popStringInstr
  flag <- popInstr
  va <- popDimVectorInstr
  vb <- liftM (subtract $ takeV dim 1) popDimVectorInstr
  let va' = va + so
      (_ : ~(_ : as)) = unVector va
      (xb : ~(yb : _)) = unVector vb
      xs = [0 .. xb]
      ys = [0 .. yb]
      cellGrid = if dim == 1
        then [[cellAt space $ va' + mkVector [x] | x <- xs]]
        else [[cellAt space $ va' + mkVector (x : y : as) | x <- xs] | y <- ys]
      mStr = liftM unlines $ mapM (mapM cellToChar) cellGrid
  case liftM2 (,) mFilepath mStr of
    Nothing -> reverseInstr
    Just (filepath, str) -> do
      cfilepath <- canonicalizePath' filepath
      outcome <- tryLiftIO $ withFile cfilepath WriteMode $ \handle -> hPutStr handle $ if testLogicalBit flag 0
        then linearize str
        else str
      when (isNothing outcome) reverseInstr

-----------------------------------------------------------
-- System Execution
-----------------------------------------------------------

executeInstr :: (I i) => Instruction i ()
executeInstr = do
  mStr <- liftM sequence popStringInstr
  case mStr of
    Nothing -> pushInstr 1
    Just str -> do
      exitCode <- liftIO $ system str
      pushInstr $ fromIntegral $ case exitCode of
        ExitSuccess -> 0
        ExitFailure k -> k

-----------------------------------------------------------
-- System Information Retrieval
-----------------------------------------------------------

getSysInfoInstr :: (I i) => Instruction i ()
getSysInfoInstr = do
  envVars <- liftIO getEnvironment
  CalendarTime {
      ctYear = year
    , ctMonth = month
    , ctDay = day
    , ctHour = hour
    , ctMin = mins
    , ctSec = sec
    } <- liftIO $ getClockTime >>= toCalendarTime
  n <- popInstr
  env <- get
  let dim = getDim env
      dim' = fromIntegral dim
      ip = currentIp env
      space = getSpace env
      progName = getProgName env
      args = getFungeArgs env
      ss = getSs ip
      (minPos, maxPos, space') = minMaxCoords space
  do
  {- 20 -} pushVectorInstr $ joinStrs $ map (\(x, y) -> x ++ "=" ++ y) envVars
  {- 19 -} pushVectorInstr $ joinStrs $ progName : args
  {- 18 -} pushVectorInstr $ mkVector $ reverse $ map (fromIntegral . Deque.depth) $ Stack.toList ss
  {- 17 -} pushInstr $ fromIntegral $ Stack.depth ss
  {- 16 -} pushInstr $ fromIntegral $ hour * 256 * 256 + mins * 256 + sec
  {- 15 -} pushInstr $ fromIntegral $ (year - 1900) * 256 * 256 + (fromEnum month + 1) * 256 + day
  {- 14 -} pushVectorInstr $ maxPos - minPos
  {- 13 -} pushVectorInstr minPos
  {- 12 -} pushVectorInstr $ getStorageOffset ip
  {- 11 -} pushVectorInstr $ getDelta ip
  {- 10 -} pushVectorInstr $ getPos ip
  {- 09 -} pushInstr 0
  {- 08 -} pushInstr $ getId ip
  {- 07 -} pushInstr $ fromIntegral dim
  {- 06 -} pushInstr $ fromIntegral $ ord pathSeparator
  {- 05 -} pushInstr 1
  {- 04 -} pushInstr $ read $ filter isDigit version
  {- 03 -} pushInstr handprint
  {- 02 -} pushInstr $ maybe (-1) fromIntegral $ byteSize n
  {- 01 -} pushInstr $ 0x01 + 0x02 + 0x04 + 0x08 + 0x10
  when (n > 0) $ do
    cell <- gets $ fromMaybe 0 . Deque.dig (n - 1) . currentToss
    put env
    pushInstr cell
  when (n <= 0 || (9 + 3 * dim' < n && n <= 9 + 5 * dim')) $ modify $ withSpace $ const space' 
  where
    joinStrs = mkVector . map (fromIntegral . ord) . ("\0\0\0" ++) . intercalate "\0" . map reverse

-----------------------------------------------------------
-- Fingerprints
-----------------------------------------------------------

fingerprintId :: (Integral i) => Vector i -> Integer
fingerprintId vec = fromIntegral $ foldl' (\fId x -> fId * 256 + x) 0 xs
  where
    xs = unVector vec

loadSemanticsInstr :: (I i) => Instruction i ()
loadSemanticsInstr = do
  count <- popInstr
  fId <- liftM (fingerprintId . reverseV) $ popVectorInstr count
  mFingerprint <- gets $ Map.lookup fId . getFingerprints
  case mFingerprint of
    Nothing -> reverseInstr
    Just fingerprint -> do
      modify $ withSemantics $ Semantics.pushFingerprint fingerprint
      pushInstr $ fromIntegral fId
      pushInstr 1

unloadSemanticsInstr :: (I i) => Instruction i ()
unloadSemanticsInstr = do
  count <- popInstr
  fId <- liftM (fingerprintId . reverseV) $ popVectorInstr count
  mFingerprint <- gets $ Map.lookup fId . getFingerprints
  case mFingerprint of
    Nothing -> reverseInstr
    Just fingerprint -> modify $ withSemantics $ Semantics.popFingerprint fingerprint

-----------------------------------------------------------
-- Concurrent Funge-98
-----------------------------------------------------------

splitInstr :: (I i) => Instruction i ()
splitInstr = do
  env <- get
  let ip = currentIp env
  case getValidIds env of
    [] -> liftIO $ do
      putStrLn "Cannot create a unique ID for new IP."
      exitFailure
    ident : idents -> let
      newIp = ip { getId = ident, getDelta = negate . getDelta $ ip }
      in modify $ const $ addSpawnedIp newIp $ env { getValidIds = idents }

