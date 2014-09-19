module Penny.Tree.Lewis where

import qualified Deka.Native.Abstract as N
import Data.Sequence (Seq)
import qualified Penny.Tree.Masuno1 as Masuno1
import qualified Penny.Core.Anna.Zero as Zero
import qualified Penny.Tree.LZ1 as LZ1

-- | The root of parse trees for number representations.

data T a
  = Novem N.Novem (Seq N.Decem) (Maybe (Masuno1.T a))
  | Zero Zero.T (Maybe (LZ1.T a))
  deriving (Eq, Ord, Show)
