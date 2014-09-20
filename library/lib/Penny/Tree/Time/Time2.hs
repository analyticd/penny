module Penny.Tree.Time.Time2 where

import qualified Penny.Tree.Space as Space
import qualified Penny.Tree.Time.Time3 as Time3
import qualified Penny.Tree.Brace.Close as Close

data T
  = End Close.T
  | Space Space.T Time3.T
  | Time3 Time3.T
  deriving (Eq, Ord, Show)
