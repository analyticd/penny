-- | Abstract, unsigned numbers.
module Penny.Anna where

import qualified Penny.Brim as Brim
import qualified Penny.Nil as Nil

data T r
  = Nil (Nil.T r)
  | Brim (Brim.T r)
  deriving (Eq, Ord, Show)
