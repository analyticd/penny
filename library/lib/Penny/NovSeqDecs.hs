module Penny.NovSeqDecs where

import qualified Penny.NovDecs as NovDecs
import qualified Penny.SeqDecs as SeqDecs

data T r = T NovDecs.T (SeqDecs.T r)
  deriving (Eq, Ord, Show)
