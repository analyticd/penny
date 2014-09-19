module Penny.Core.Anna.NovSeqDecs where

import qualified Penny.Core.Anna as NovDecs
import qualified Penny.Core.Anna.SeqDecs as SeqDecs

data T r = T NovDecs.T (SeqDecs.T r)
  deriving (Eq, Ord, Show)
