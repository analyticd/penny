module Penny.Tree.SubAccount.Quoted where

import qualified Penny.Tree.SubAccount.Quoted.Char as Char
import Data.Sequence (Seq)

data T = T { toSeq :: Seq Char.T }
  deriving (Eq, Ord, Show)
