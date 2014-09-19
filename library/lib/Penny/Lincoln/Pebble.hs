module Penny.Lincoln.Pebble where

import qualified Penny.Lincoln.Gravel as Gravel
import qualified Penny.Lincoln.Side as Side
import qualified Penny.Lincoln.Cement as Cement

newtype T = T { toGravel :: Gravel.T Side.T }
  deriving (Eq, Ord, Show)

fromGravel :: Gravel.T Side.T -> T
fromGravel = T

toCement :: T -> Cement.T
toCement = Gravel.toCement Side.toSign . toGravel

fromCement :: Cement.T -> T
fromCement = T . Gravel.fromCement Side.fromSign
