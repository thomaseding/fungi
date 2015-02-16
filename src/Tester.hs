module Tester (
    main
  ) where

import Control.Exception (handle)
import Control.Monad

import Data.List (sort, isSuffixOf)

import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
import System.Environment (withArgs)
import System.Exit (ExitCode (..), exitSuccess, exitFailure)

import qualified Fungi

-----------------------------------------------------------

main :: IO ExitCode
main = do
  putStrLn "Running Tests..."
  (goodPasses, goodFailures) <- runTests goodTestDir
  (badFailures, badPasses) <- runTests badTestDir
  putStrLn "-----------------------------------------------------------"
  putStrLn $ "Good Passes: " ++ show goodPasses
  putStrLn $ "Good Failures: " ++ show goodFailures
  putStrLn "-----------------------------------------------------------"
  putStrLn $ "Bad Passes: " ++ show badPasses
  putStrLn $ "Bad Failures: " ++ show badFailures
  putStrLn "-----------------------------------------------------------"
  putStrLn $ "Total Tests Passed: " ++ show (goodPasses + badPasses)
  putStrLn $ "Total Tests Failed: " ++ show (goodFailures + badFailures)
  if (goodFailures + badFailures) > 0
    then return $ ExitFailure 1
    else return ExitSuccess
  where
    goodTestDir = "../tests/good/"
    badTestDir = "../tests/bad/"

getDirectoryFilesRecursive :: FilePath -> IO [FilePath]
getDirectoryFilesRecursive path = do
  contents <- liftM (filter (`notElem` [".", ".."])) $ getDirectoryContents path
  dirs <- filterM doesDirectoryExist $ map ((path ++) . (++ "/")) contents
  files <- filterM doesFileExist $ map (path ++) contents
  files' <- mapM getDirectoryFilesRecursive dirs
  return $ files ++ concat files'

runTests :: FilePath -> IO (Int, Int)
runTests testDir = do
  files <- return
    . sort 
    . filter (\file -> any (`isSuffixOf` file) [".uf", ".u98", ".bf", ".b98", ".tf", ".t98"])
    =<< getDirectoryFilesRecursive testDir
  exitCodes <- mapM test files
  let numSuccesses = countSuccesses exitCodes
      numFailures = countFailures exitCodes
  return (numSuccesses, numFailures)

countSuccesses :: [ExitCode] -> Int
countSuccesses = length . filter (== ExitSuccess)

countFailures :: [ExitCode] -> Int
countFailures = length . filter (/= ExitSuccess)

printOnFail :: FilePath -> ExitCode -> IO ExitCode
printOnFail file exitCode@(ExitFailure n) = do
  putStrLn $ "Fail: " ++ file ++ ", with exit code " ++ show n
  return exitCode
printOnFail _ exitCode = return exitCode

test :: FilePath -> IO ExitCode
test file = do
  putStrLn $ ">>>> Testing: " ++ file
  exitCode <- handle return $ withArgs args Fungi.main
  putStr "\n>>>> "
  print exitCode
  return exitCode
  where
    args = ["--debug", "0", file]


