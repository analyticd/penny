module Penny.Tree.Digits.D1or2 where

import qualified Penny.Tree.Digit as D

data T = T D.T (Maybe D.T)
  deriving (Eq, Ord, Show)
