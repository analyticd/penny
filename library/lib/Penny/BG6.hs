module Penny.BG6 where

import qualified Penny.NovSeqDecsNE as NovSeqDecsNE
import qualified Penny.BG7 as BG7

data T r
  = Novem (NovSeqDecsNE.T r)
  | Group r (BG7.T r)
  deriving (Eq, Ord, Show)
