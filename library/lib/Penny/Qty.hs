module Penny.Qty where

import qualified Penny.Concrete as Concrete
import qualified Penny.Pebble as Pebble

newtype T = T { toConcrete :: Concrete.T }
  deriving (Eq, Ord, Show)

fromConcrete :: Concrete.T -> T
fromConcrete = T

fromPebble :: Pebble.T -> T
fromPebble = T . Concrete.fromCement . Pebble.toCement

toPebble :: T -> Pebble.T
toPebble = Pebble.fromCement . Concrete.toCement . toConcrete
