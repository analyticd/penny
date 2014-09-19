module Penny.Core.Anna.NG1 where

import qualified Penny.Core.Anna.Radix as Radix
import qualified Penny.Core.Anna.Zeroes as Zeroes
import Data.Sequence (Seq)
import qualified Penny.Core.Anna.ZGroup as ZGroup

data T r = T
  { radix :: Radix.T r
  , zeroes1 :: Zeroes.T
  , grouper1 :: r
  , zeroes2 :: Zeroes.T
  , groups :: Seq (ZGroup.T r)
  } deriving (Eq, Ord, Show)
