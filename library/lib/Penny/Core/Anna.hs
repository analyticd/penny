module Penny.Core.Anna where

import qualified Penny.Core.Anna.Nil as Nil
import qualified Penny.Core.Anna.Brim as Brim

data T a
  = Nil (Nil.T a)
  | Brim (Brim.T a)
  deriving (Eq, Ord, Show)
