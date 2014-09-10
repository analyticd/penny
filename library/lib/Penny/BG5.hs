module Penny.BG5 where

import qualified Penny.NovSeqDecsNE as NovSeqDecsNE
import qualified Penny.BG6 as BG6
import qualified Penny.Zeroes as Zeroes

data T r
  = Novem (NovSeqDecsNE.T r)
  | Zeroes Zeroes.T (BG6.T r)
  deriving (Eq, Ord, Show)
