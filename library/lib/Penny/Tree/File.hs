module Penny.Tree.File where

import Data.Sequence (Seq)
import qualified Penny.Tree.EOF as EOF
import qualified Penny.Tree.Line as Line

data T = T (Seq Line.T) EOF.T
  deriving (Eq, Ord, Show)
