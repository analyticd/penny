module Penny.Qty where

import qualified Penny.Concrete as Concrete
import qualified Penny.Pebble as Pebble

newtype T = T { toConcrete :: Concrete.T }
  deriving (Eq, Ord, Show)
