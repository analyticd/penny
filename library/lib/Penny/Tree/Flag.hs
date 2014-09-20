module Penny.Tree.Flag where

import qualified Penny.Tree.Square.Open as Open
import qualified Penny.Tree.Square.Close as Close
import Data.Sequence (Seq)
import qualified Penny.Tree.Flag.Char as Char

data T = T Open.T (Seq Char.T) Close.T
  deriving (Eq, Ord, Show)
