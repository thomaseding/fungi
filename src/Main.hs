module Main (
    main
  )
  where

import System.Exit

import qualified Fungi

-----------------------------------------------------------

main :: IO ()
main = Fungi.main >>= exitWith

