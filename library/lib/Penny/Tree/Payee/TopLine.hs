module Penny.Tree.Payee.TopLine where

import qualified Penny.Tree.Payee.TopLine.Char.First as First
import qualified Penny.Tree.Payee.TopLine.Char.Next as Next
import Data.Sequence (Seq)

data T = T
  { first :: First.T
  , rest :: Seq Next.T
  } deriving (Eq, Ord, Show)
