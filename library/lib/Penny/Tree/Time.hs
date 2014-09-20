module Penny.Tree.Time where

import qualified Penny.Tree.Brace.Open as Open
import qualified Penny.Tree.Colon as Colon
import qualified Penny.Tree.Digits.D1or2 as D2
import qualified Penny.Tree.Time.Time2 as Time2

data T = T Open.T D2.T Colon.T D2.T Time2.T
  deriving (Eq, Ord, Show)
