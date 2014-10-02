module Penny.Core.Transaction.Error where

import qualified Penny.Core.Trio.Error as Trio.Error
import qualified Penny.Core.Posting as Posting
import qualified Penny.Core.Imbalances as Imbalances

data T
  = Trio Trio.Error.T Posting.T
  | Imbalanced Imbalances.T
  deriving (Eq, Ord, Show)
