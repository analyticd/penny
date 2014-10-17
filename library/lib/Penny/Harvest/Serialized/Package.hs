module Penny.Harvest.Serialized.Package where

import qualified Penny.Harvest.Serialized.Item as Item
import qualified Penny.Core.Clxn as Clxn
import qualified Penny.Harvest.Zoned.Located as Located
import Data.Sequence (Seq)

data T = T
  { clxn :: Clxn.T
  , items :: Seq (Located.T Item.T)
  } deriving (Eq, Ord, Show)
