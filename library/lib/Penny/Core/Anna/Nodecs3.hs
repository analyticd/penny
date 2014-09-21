module Penny.Core.Anna.Nodecs3 where

import qualified Penny.Core.Anna.SeqDecs as SeqDecs
import qualified Penny.Core.Anna.NovDecs as NovDecs

data T r = T
  { novDecs :: NovDecs.T
  , seqDecs :: SeqDecs.T r
  } deriving (Eq, Ord, Show)
