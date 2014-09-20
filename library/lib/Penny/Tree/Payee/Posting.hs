module Penny.Tree.Payee.Posting where

import qualified Penny.Tree.Tilde as Tilde
import Data.Sequence (Seq)
import qualified Penny.Tree.Payee.Posting.Char as Char

data T = T Tilde.T (Seq Char.T) Tilde.T
  deriving (Eq, Ord, Show)
