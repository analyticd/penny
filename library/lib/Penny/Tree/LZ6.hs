module Penny.Tree.LZ6 where

import qualified Penny.Core.Anna as NovDecs
import qualified Penny.Core.Anna.DecDecs as DecDecs
import qualified Penny.Core.Anna.Zeroes as Zeroes
import Data.Sequence (Seq)

data T a
  = Novem NovDecs.T (Seq (a, DecDecs.T))
  | Zero Zeroes.T
         (Maybe (Either (NovDecs.T, Seq (a, DecDecs.T))
                        (a, T a)))
  deriving (Eq, Ord, Show)
