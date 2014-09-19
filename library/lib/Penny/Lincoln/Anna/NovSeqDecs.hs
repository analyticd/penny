module Penny.Lincoln.Anna.NovSeqDecs where

import qualified Penny.Lincoln.Anna as NovDecs
import qualified Penny.Lincoln.Anna.SeqDecs as SeqDecs

data T r = T NovDecs.T (SeqDecs.T r)
  deriving (Eq, Ord, Show)
