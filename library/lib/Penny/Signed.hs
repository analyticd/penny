module Penny.Signed where

import qualified Penny.Polarity as Polarity
import qualified Penny.Nil as Nil
import qualified Penny.Brim as Brim

newtype T r p
  = T { toPolarity :: Polarity.T (Nil.T r) (Brim.T r) p }
  deriving (Eq, Ord, Show)
