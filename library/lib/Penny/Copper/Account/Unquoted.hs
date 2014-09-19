module Penny.Copper.Account.Unquoted where

import qualified Penny.Copper.SubAccount.Unquoted.First as First
import qualified Penny.Copper.SubAccount.Unquoted.Next as Next
import Data.Sequence (Seq)

data T = T First.T (Seq Next.T)
  deriving (Eq, Ord, Show)
