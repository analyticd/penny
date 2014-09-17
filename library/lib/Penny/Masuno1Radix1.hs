module Penny.Masuno1Radix1 where

import qualified Deka.Native.Abstract as N
import Data.Sequence (Seq)

data T a
  = T N.Decem (Seq N.Decem) (Seq (a, N.Decem, (Seq N.Decem)))
  deriving (Eq, Ord, Show)
