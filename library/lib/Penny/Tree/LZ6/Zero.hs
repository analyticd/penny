module Penny.Tree.LZ6.Zero where

import qualified Penny.Core.Anna.NovDecs as NovDecs
import qualified Penny.Core.Anna.SeqDecs as SeqDecs
import qualified Penny.Core.Anna.Zeroes as Zeroes
import Prelude hiding (foldr)
import qualified Penny.Tree.LZ6 as LZ6

-- | Zero value when folding.
data T a
  = Novem NovDecs.T (SeqDecs.T a)
  | ZeroOnly Zeroes.T
  | ZeroNovSeq Zeroes.T NovDecs.T (SeqDecs.T a)
  deriving (Eq, Ord, Show)

