module Penny.Nodecs3 where

import qualified Penny.SeqDecs as SeqDecs
import qualified Penny.NovDecs as NovDecs

data T r = T
  { novDecs :: NovDecs.T
  , seqDecs :: SeqDecs.T r
  } deriving (Eq, Ord, Show)
