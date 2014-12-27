module Penny.Lincoln.Commodity where

import Data.Text

newtype Commodity = Commodity Text
  deriving (Eq, Ord, Show)
