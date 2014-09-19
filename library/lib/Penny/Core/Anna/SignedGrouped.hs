module Penny.Core.Anna.SignedGrouped where

import qualified Penny.Core.Polarity as Polarity
import qualified Penny.Core.Anna.Nil.Grouped as NilGrouped
import qualified Penny.Core.Anna.BrimGrouped as BrimGrouped

data T r p = T
  { toPolarity :: Polarity.T (NilGrouped.T r) (BrimGrouped.T r) p }
  deriving (Eq, Ord, Show)
