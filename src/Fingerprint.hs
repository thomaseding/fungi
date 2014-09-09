module Fingerprint (
    Fingerprints
  , fingerprints
  ) where

import Data.Char (ord)
import Data.IntegralLike
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tuple.Map

import Instruction

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

-----------------------------------------------------------

type Fingerprints i = Map Integer [(i, Instruction i ())]

asId :: String -> Integer
asId = fromIntegral . foldl' (\fId x -> fId * 256 + x) 0 . map ord

fingerprints :: (I i) => Fingerprints i
fingerprints = foldr (uncurry (insert . asId) . map2 (map $ map1 asIntegral)) Map.empty [
    (BASE.name, BASE.semantics)
  , (BF93.name, BF93.semantics)
  , (BOOL.name, BOOL.semantics)
  , (BZRO.name, BZRO.semantics)
  , (CPLI.name, CPLI.semantics)
  , (FIXP.name, FIXP.semantics)
  , (HRTI.name, HRTI.semantics)
  , (MODE.name, MODE.semantics)
  , (MODU.name, MODU.semantics)
  , (NOP.name,  NOP.semantics )
  , (NULL.name, NULL.semantics)
  , (ORTH.name, ORTH.semantics)
  , (RECD.name, RECD.semantics)
  , (REFC.name, REFC.semantics)
  , (ROMA.name, ROMA.semantics)
  , (STRN.name, STRN.semantics)
  ]
  where
    insert k v m = if Map.member k m
      then error $ "Fingerprint.fingerprints: Duplicate entry for fingerprint " ++ show k
      else Map.insert k v m

