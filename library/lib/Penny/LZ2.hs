module Penny.LZ2 where

import qualified Penny.NovDecs as NovDecs
import qualified Penny.Zeroes as Zeroes
import qualified Penny.LZ3 as LZ3
import qualified Penny.LZ4 as LZ4

data T a
  = Zero Zeroes.T (Maybe (LZ3.T a))
  | Novem NovDecs.T (Maybe (LZ4.T a))
  deriving (Eq, Ord, Show)
