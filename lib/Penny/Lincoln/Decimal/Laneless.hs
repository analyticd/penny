module Penny.Lincoln.Decimal.Laneless where

import Penny.Lincoln.Decimal.Abstract
import Penny.Lincoln.Decimal.Zero

data Laneless
  = LNZ NonZero
  | LZ Zero
  deriving (Eq, Ord, Show)

data Lessrad = Lessrad
  { lrLaneless :: Laneless
  , lrRadGroup :: RadGroup
  } deriving (Eq, Show, Ord)
