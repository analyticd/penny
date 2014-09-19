module Penny.Copper.Account.Quoted where

import Data.Sequence (Seq)
import qualified Penny.Copper.SubAccount.Quoted.First as First
import qualified Penny.Copper.SubAccount.Quoted.Next as Next
import qualified Penny.Copper.Brace.Open as Open
import qualified Penny.Copper.Brace.Close as Close

data T = T Open.T First.T (Seq Next.T) Close.T
  deriving (Eq, Ord, Show)
