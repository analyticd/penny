module Penny.Price.Stone where

import qualified Penny.Lincoln.Gravel as Gravel
import qualified Penny.Price.PluMin as PluMin
import qualified Penny.Lincoln.Cement as Cement

newtype T = T { toGravel :: Gravel.T PluMin.T }
  deriving (Eq, Ord, Show)

fromGravel :: Gravel.T PluMin.T -> T
fromGravel = T

toCement :: T -> Cement.T
toCement = Gravel.toCement PluMin.toSign . toGravel

fromCement :: Cement.T -> T
fromCement = T . Gravel.fromCement PluMin.fromSign
