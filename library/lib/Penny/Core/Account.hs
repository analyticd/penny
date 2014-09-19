module Penny.Core.Account where

import Data.Sequence
import qualified Penny.Core.SubAccount as SubAccount

data T = T { toSeq :: Seq SubAccount.T }
  deriving (Eq, Ord, Show)
