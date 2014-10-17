module Penny.Harvest.Error where

import qualified Penny.Harvest.Error.Ding as Ding
import Data.Sequence (Seq)

data T = T
  { first :: Ding.T
  , rest :: Seq Ding.T
  } deriving Show
