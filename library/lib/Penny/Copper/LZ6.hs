module Penny.Copper.LZ6 where

import qualified Penny.Lincoln.Anna as NovDecs
import qualified Penny.Lincoln.Anna.DecDecs as DecDecs
import qualified Penny.Zeroes as Zeroes
import Data.Sequence (Seq)

data T a
  = Novem NovDecs.T (Seq (a, DecDecs.T))
  | Zero Zeroes.T
         (Maybe (Either (NovDecs.T, Seq (a, DecDecs.T))
                        (a, T a)))
  deriving (Eq, Ord, Show)
