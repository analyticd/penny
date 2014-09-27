module Penny.Tree.Time.Error where

import qualified Penny.Tree.Digits.D1or2 as D2
import qualified Penny.Tree.Digits.D4 as D4
import qualified Penny.Tree.Hyphen as Hyphen
import qualified Penny.Tree.Plus as Plus

-- | Errors when converting a time to a core format.
data T
  = BadHours D2.T
  | BadMinutes D2.T
  | BadSeconds D2.T
  | BadOffset (Either Hyphen.T Plus.T) D4.T
  deriving (Eq, Ord, Show)
