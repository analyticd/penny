module Penny.Lincoln.Side where

import Penny.Lincoln.Offset
import Penny.Lincoln.PluMin
import Penny.Lincoln.Display

data Side = Debit | Credit
  deriving (Eq, Ord, Show)

instance Display Side where
  display Debit = ('<':)
  display Credit = ('>':)

instance HasOffset Side where
  offset Debit = Credit
  offset Credit = Debit

instance Signed Side where
  sign Debit = Minus
  sign Credit = Plus
  fromSign Minus = Debit
  fromSign Plus = Credit
