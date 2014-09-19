module Penny.Core.Anna.SignedUngrouped where

import qualified Penny.Core.Polarity as Polarity
import qualified Penny.Core.Anna.Nil.Ungrouped as NilUngrouped
import qualified Penny.Core.Anna.BrimUngrouped as BrimUngrouped

newtype T r p = T
  { toPolarity :: Polarity.T (NilUngrouped.T r) (BrimUngrouped.T r) p }
  deriving (Eq, Ord, Show)
