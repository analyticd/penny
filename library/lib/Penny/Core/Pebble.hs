module Penny.Core.Pebble where

import qualified Penny.Core.Gravel as Gravel
import qualified Penny.Core.Side as Side
import qualified Penny.Core.Cement as Cement

newtype T = T { toGravel :: Gravel.T Side.T }
  deriving (Eq, Ord, Show)

fromGravel :: Gravel.T Side.T -> T
fromGravel = T

toCement :: T -> Cement.T
toCement = Gravel.toCement Side.toSign . toGravel

fromCement :: Cement.T -> T
fromCement = T . Gravel.fromCement Side.fromSign
