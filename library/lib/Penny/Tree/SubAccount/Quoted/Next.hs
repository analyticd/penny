module Penny.Tree.SubAccount.Quoted.Next where

import qualified Penny.Tree.SubAccount.Quoted.Char as Char
import qualified Penny.Tree.Colon as Colon
import Data.Sequence (Seq)

data T = T Colon.T (Seq Char.T)
  deriving (Eq, Ord, Show)
