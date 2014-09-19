module Penny.Lincoln.Anna.NovSeqDecsNE where

import qualified Penny.Lincoln.Anna as NovDecs
import qualified Penny.Lincoln.Anna.SeqDecsNE as SeqDecsNE

data T r = T
  { novDecs :: NovDecs.T
  , seqDecsNE :: SeqDecsNE.T r
  } deriving (Eq, Ord, Show)
