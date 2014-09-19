module Penny.Lincoln.Anna.BG6 where

import qualified Penny.Lincoln.Anna.NovSeqDecsNE as NovSeqDecsNE
import qualified Penny.Lincoln.Anna.BG7 as BG7

data T r
  = Novem (NovSeqDecsNE.T r)
  | Group r (BG7.T r)
  deriving (Eq, Ord, Show)
