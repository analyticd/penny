module Penny.Tree.LZ2 where

import qualified Penny.Core.Anna.NovDecs as NovDecs
import qualified Penny.Core.Anna.Zeroes as Zeroes
import qualified Penny.Tree.LZ3 as LZ3
import qualified Penny.Tree.LZ4 as LZ4

data T a
  = Zero Zeroes.T (Maybe (LZ3.T a))
  | Novem NovDecs.T (Maybe (LZ4.T a))
  deriving (Eq, Ord, Show)
