module Penny.Copper.Lewis where

import qualified Deka.Native.Abstract as N
import Data.Sequence (Seq)
import qualified Penny.Copper.Masuno1 as Masuno1
import qualified Penny.Zero as Zero
import qualified Penny.Copper.LZ1 as LZ1

-- | The root of parse trees for number representations.

data T a
  = Novem N.Novem (Seq N.Decem) (Maybe (Masuno1.T a))
  | Zero Zero.T (Maybe (LZ1.T a))
  deriving (Eq, Ord, Show)
