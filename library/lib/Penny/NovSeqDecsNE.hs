module Penny.NovSeqDecsNE where

import qualified Penny.NovDecs as NovDecs
import qualified Penny.SeqDecsNE as SeqDecsNE

data T r = T
  { novDecs :: NovDecs.T
  , seqDecsNE :: SeqDecsNE.T r
  } deriving (Eq, Ord, Show)
