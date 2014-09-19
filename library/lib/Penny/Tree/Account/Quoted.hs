module Penny.Tree.Account.Quoted where

import Data.Sequence (Seq)
import qualified Penny.Tree.SubAccount.Quoted.First as First
import qualified Penny.Tree.SubAccount.Quoted.Next as Next
import qualified Penny.Tree.Brace.Open as Open
import qualified Penny.Tree.Brace.Close as Close

data T = T Open.T First.T (Seq Next.T) Close.T
  deriving (Eq, Ord, Show)
