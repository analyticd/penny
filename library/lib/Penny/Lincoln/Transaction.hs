module Penny.Lincoln.Transaction where

import Penny.Lincoln.Ents
import Penny.Lincoln.Field
import Data.Map (Map)

data Transaction
  = Transaction (Map Label Field) (Balanced (Map Label Field))
  deriving (Eq, Ord, Show)
