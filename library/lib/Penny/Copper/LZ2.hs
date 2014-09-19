module Penny.Copper.LZ2 where

import qualified Penny.Lincoln.Anna as NovDecs
import qualified Penny.Lincoln.Anna.Zeroes as Zeroes
import qualified Penny.Copper.LZ3 as LZ3
import qualified Penny.Copper.LZ4 as LZ4

data T a
  = Zero Zeroes.T (Maybe (LZ3.T a))
  | Novem NovDecs.T (Maybe (LZ4.T a))
  deriving (Eq, Ord, Show)
