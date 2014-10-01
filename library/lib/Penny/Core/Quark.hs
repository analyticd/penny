module Penny.Core.Quark where

import qualified Penny.Core.Quant as Quant
import qualified Penny.Core.Side as Side
import qualified Penny.Core.Pebble as Pebble

newtype T = T { toQuant :: Quant.T Side.T }
  deriving (Eq, Ord, Show)

fromQuant :: Quant.T Side.T -> T
fromQuant = T

fromPebble :: Pebble.T -> Maybe T
fromPebble = fmap T . Quant.fromGravel . Pebble.toGravel

toPebble :: T -> Pebble.T
toPebble = Pebble.fromGravel . Quant.toGravel . toQuant

offset :: T -> T
offset (T (Quant.T n e s)) = T (Quant.T n e (Side.opposite s))

side :: T -> Side.T
side = Quant.side . toQuant
