module Penny.LZ3 where

import qualified Penny.NovDecs as NovDecs
import qualified Penny.LZ4 as LZ4
import qualified Penny.LZ6 as LZ6

data T a
  = Novem NovDecs.T (Maybe (LZ4.T a))
  | Group a (LZ6.T a)
  deriving (Eq, Ord, Show)
