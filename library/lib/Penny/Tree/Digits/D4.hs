module Penny.Tree.Digits.D4 where

import qualified Penny.Tree.Digit as D

data T = T D.T D.T D.T D.T
  deriving (Eq, Ord, Show)
