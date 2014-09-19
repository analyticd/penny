module Penny.Lincoln.Stokely where

import qualified Penny.Polarity as Polarity
import qualified Penny.Lincoln.Anna.Nil as Nil
import qualified Penny.Lincoln.Brim as Brim

newtype T r p
  = T { toPolarity :: Polarity.T (Nil.T r) (Brim.T r) p }
  deriving (Eq, Ord, Show)
