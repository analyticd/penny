module Penny.Lincoln.Account where

import Data.Sequence
import qualified Penny.SubAccount as SubAccount

data T = T { toSeq :: Seq SubAccount.T }
  deriving (Eq, Ord, Show)
