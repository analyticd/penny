module Penny.Tree.Account.Unquoted where

import qualified Penny.Tree.SubAccount.Unquoted.First as First
import qualified Penny.Tree.SubAccount.Unquoted.Next as Next
import Data.Sequence (Seq)

data T = T First.T (Seq Next.T)
  deriving (Eq, Ord, Show)
