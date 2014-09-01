module Penny.Copper.Tree.Digits where

import Penny.Copper.Tree.Digit

data Digits4 = Digits4 Digit Digit Digit Digit
  deriving (Eq, Ord, Show)

data Digits1or2 = Digits1or2 Digit (Maybe Digit)
  deriving (Eq, Ord, Show)
