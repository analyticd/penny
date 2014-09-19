module Penny.Tree.SubAccount.Quoted.First where

import qualified Penny.Tree.SubAccount.Quoted.Char as Char
import Data.Sequence (Seq)

data T = T (Seq Char.T)
  deriving (Eq, Ord, Show)
