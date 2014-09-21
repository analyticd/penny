module Penny.Tree.Side where

import qualified Penny.Tree.Than.Less as Less
import qualified Penny.Tree.Than.Greater as Greater

data T
  = Debit Less.T
  | Greater Greater.T
  deriving (Eq, Ord, Show)
