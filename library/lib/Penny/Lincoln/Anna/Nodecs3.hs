module Penny.Lincoln.Anna.Nodecs3 where

import qualified Penny.Lincoln.Anna.SeqDecs as SeqDecs
import qualified Penny.Lincoln.Anna as NovDecs

data T r = T
  { novDecs :: NovDecs.T
  , seqDecs :: SeqDecs.T r
  } deriving (Eq, Ord, Show)
