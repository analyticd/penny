module Penny.Quark where

import qualified Penny.Quant as Q
import qualified Penny.Lincoln.Side as Side
import qualified Penny.Pebble as Pebble

newtype T = T { toQuant :: Q.T Side.T }
  deriving (Eq, Ord, Show)

fromQuant :: Q.T Side.T -> T
fromQuant = T

fromPebble :: Pebble.T -> Maybe T
fromPebble = fmap T . Q.fromGravel . Pebble.toGravel

toPebble :: T -> Pebble.T
toPebble = Pebble.fromGravel . Q.toGravel . toQuant
