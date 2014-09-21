module Penny.Core.Stokely where

import qualified Penny.Core.Polarity as Polarity
import qualified Penny.Core.Anna.Nil as Nil
import qualified Penny.Core.Anna.Brim as Brim

newtype T r p
  = T { toPolarity :: Polarity.T (Nil.T r) (Brim.T r) p }
  deriving (Eq, Ord, Show)
