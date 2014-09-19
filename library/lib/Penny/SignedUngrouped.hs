module Penny.SignedUngrouped where

import qualified Penny.Polarity as Polarity
import qualified Penny.Lincoln.Anna.Nil.Ungrouped as NilUngrouped
import qualified Penny.Lincoln.Anna.BrimUngrouped as BrimUngrouped

newtype T r p = T
  { toPolarity :: Polarity.T (NilUngrouped.T r) (BrimUngrouped.T r) p }
  deriving (Eq, Ord, Show)
