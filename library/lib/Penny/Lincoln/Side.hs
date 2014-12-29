module Penny.Lincoln.Side where

import Penny.Lincoln.Offset

data Side = Debit | Credit
  deriving (Eq, Ord, Show)

instance HasOffset Side where
  offset Debit = Credit
  offset Credit = Debit
