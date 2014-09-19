module Penny.Tree.LZ4 where

import qualified Penny.Core.Anna.DecDecs as DecDecs
import Data.Sequence (Seq)

data T a = T a DecDecs.T (Seq (a, DecDecs.T))
  deriving (Eq, Ord, Show)
