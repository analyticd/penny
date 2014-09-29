module Penny.Tree.LZ6.Zero where

import qualified Penny.Core.NovDecs as NovDecs
import qualified Penny.Core.Anna.SeqDecsNE as SeqDecsNE
import qualified Penny.Core.Anna.Zeroes as Zeroes
import Prelude hiding (foldr)

-- | Zero value when folding.
data T a
  = Novem NovDecs.T (Maybe (SeqDecsNE.T a))
  | ZeroOnly Zeroes.T
  | ZeroNovSeq Zeroes.T NovDecs.T (Maybe (SeqDecsNE.T a))
  deriving (Eq, Ord, Show)

