module Penny.SignedUngrouped where

import qualified Penny.Polarity as Polarity
import qualified Penny.NilUngrouped as NilUngrouped
import qualified Penny.BrimUngrouped as BrimUngrouped

newtype T r p = T
  { toPolarity :: Polarity.T (NilUngrouped.T r) (BrimUngrouped.T r) p }
  deriving (Eq, Ord, Show)
