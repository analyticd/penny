module Penny.Tree.SubAccount.Unquoted.First where

import qualified Penny.Tree.SubAccount.Unquoted.Char.First as FirstC
import qualified Penny.Tree.SubAccount.Unquoted.Char.Next as NextC
import Data.Sequence (Seq)

data T = T
  { first :: FirstC.T
  , rest :: Seq NextC.T
  } deriving (Eq, Ord, Show)
