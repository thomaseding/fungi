module Text.Help.Fingerprint (
    help
  ) where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import qualified Fingerprint.BASE as BASE
import qualified Fingerprint.BF93 as BF93
import qualified Fingerprint.BOOL as BOOL
import qualified Fingerprint.BZRO as BZRO
import qualified Fingerprint.CPLI as CPLI
import qualified Fingerprint.FIXP as FIXP
import qualified Fingerprint.HRTI as HRTI
import qualified Fingerprint.MODE as MODE
import qualified Fingerprint.MODU as MODU
import qualified Fingerprint.NOP  as NOP
import qualified Fingerprint.NULL as NULL
import qualified Fingerprint.ORTH as ORTH
import qualified Fingerprint.RECD as RECD
import qualified Fingerprint.REFC as REFC
import qualified Fingerprint.ROMA as ROMA
import qualified Fingerprint.STRN as STRN

import qualified Text.Help.Fingerprint.BASE as H_BASE
import qualified Text.Help.Fingerprint.BF93 as H_BF93
import qualified Text.Help.Fingerprint.BOOL as H_BOOL
import qualified Text.Help.Fingerprint.BZRO as H_BZRO
import qualified Text.Help.Fingerprint.CPLI as H_CPLI
import qualified Text.Help.Fingerprint.FIXP as H_FIXP
import qualified Text.Help.Fingerprint.HRTI as H_HRTI
import qualified Text.Help.Fingerprint.MODE as H_MODE
import qualified Text.Help.Fingerprint.MODU as H_MODU
import qualified Text.Help.Fingerprint.NOP  as H_NOP
import qualified Text.Help.Fingerprint.NULL as H_NULL
import qualified Text.Help.Fingerprint.ORTH as H_ORTH
import qualified Text.Help.Fingerprint.RECD as H_RECD
import qualified Text.Help.Fingerprint.REFC as H_REFC
import qualified Text.Help.Fingerprint.ROMA as H_ROMA
import qualified Text.Help.Fingerprint.STRN as H_STRN

-----------------------------------------------------------

line :: IO ()
line = putStrLn $ replicate 80 '-'

noHelp :: String -> IO ()
noHelp finger = putStrLn $ "Unable to find fingerprint documentation for " ++ finger

help :: String -> IO ()
help finger = do
  line
  doc
  line
  where
    doc = fromMaybe (noHelp finger) $ Map.lookup finger $ Map.fromList [
        (BASE.name, H_BASE.help)
      , (BF93.name, H_BF93.help)
      , (BOOL.name, H_BOOL.help)
      , (BZRO.name, H_BZRO.help)
      , (CPLI.name, H_CPLI.help)
      , (FIXP.name, H_FIXP.help)
      , (HRTI.name, H_HRTI.help)
      , (MODE.name, H_MODE.help)
      , (MODU.name, H_MODU.help)
      , (NOP.name , H_NOP.help )
      , (NULL.name, H_NULL.help)
      , (ORTH.name, H_ORTH.help)
      , (RECD.name, H_RECD.help)
      , (REFC.name, H_REFC.help)
      , (ROMA.name, H_ROMA.help)
      , (STRN.name, H_STRN.help)
      ]

