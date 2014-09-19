module Penny.Lincoln.Anna.BG5 where

import qualified Penny.Lincoln.Anna.NovSeqDecsNE as NovSeqDecsNE
import qualified Penny.Lincoln.Anna.BG6 as BG6
import qualified Penny.Lincoln.Anna.Zeroes as Zeroes

data T r
  = Novem (NovSeqDecsNE.T r)
  | Zeroes Zeroes.T (BG6.T r)
  deriving (Eq, Ord, Show)
