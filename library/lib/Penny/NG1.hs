module Penny.NG1 where

import qualified Penny.Radix as Radix
import qualified Penny.Zeroes as Zeroes
import Data.Sequence (Seq)
import qualified Penny.ZGroup as ZGroup

data T r = T
  { radix :: Radix.T r
  , zeroes1 :: Zeroes.T
  , grouper1 :: r
  , zeroes2 :: Zeroes.T
  , groups :: Seq (ZGroup.T r)
  } deriving (Eq, Ord, Show)
