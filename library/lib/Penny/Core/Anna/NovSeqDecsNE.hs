module Penny.Core.Anna.NovSeqDecsNE where

import qualified Penny.Core.Anna as NovDecs
import qualified Penny.Core.Anna.SeqDecsNE as SeqDecsNE

data T r = T
  { novDecs :: NovDecs.T
  , seqDecsNE :: SeqDecsNE.T r
  } deriving (Eq, Ord, Show)
