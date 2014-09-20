module Penny.Tree.Number where

import qualified Penny.Tree.Paren.Open as Open
import qualified Penny.Tree.Paren.Close as Close
import Data.Sequence (Seq)
import qualified Penny.Tree.Number.Char as Char

data T = T Open.T (Seq Char.T) Close.T
  deriving (Eq, Ord, Show)
