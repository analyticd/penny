module Penny.Anna.NovSeqDecs where

import qualified Penny.Lincoln.Anna as NovDecs
import qualified Penny.SeqDecs as SeqDecs

data T r = T NovDecs.T (SeqDecs.T r)
  deriving (Eq, Ord, Show)
