module Penny.Core.Stone where

import qualified Penny.Core.Gravel as Gravel
import qualified Penny.Core.PluMin as PluMin
import qualified Penny.Core.Cement as Cement

newtype T = T { toGravel :: Gravel.T PluMin.T }
  deriving (Eq, Ord, Show)

fromGravel :: Gravel.T PluMin.T -> T
fromGravel = T

toCement :: T -> Cement.T
toCement = Gravel.toCement PluMin.toSign . toGravel

fromCement :: Cement.T -> T
fromCement = T . Gravel.fromCement PluMin.fromSign
