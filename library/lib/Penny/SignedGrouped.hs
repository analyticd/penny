module Penny.SignedGrouped where

import qualified Penny.Polarity as Polarity
import qualified Penny.NilGrouped as NilGrouped
import qualified Penny.BrimGrouped as BrimGrouped

data T r p = T
  { toPolarity :: Polarity.T (NilGrouped.T r) (BrimGrouped.T r) p }
  deriving (Eq, Ord, Show)
