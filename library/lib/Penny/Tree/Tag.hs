module Penny.Tree.Tag where

import qualified Penny.Tree.Asterisk as Asterisk
import Data.Sequence (Seq)
import qualified Penny.Tree.Tag.Char as Char

data T = T Asterisk.T (Seq Char.T)
  deriving (Eq, Ord, Show)
