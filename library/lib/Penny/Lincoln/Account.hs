module Penny.Lincoln.Account where

import Data.Sequence
import qualified Penny.Lincoln.SubAccount as SubAccount

data T = T { toSeq :: Seq SubAccount.T }
  deriving (Eq, Ord, Show)
