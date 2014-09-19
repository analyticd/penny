module Penny.Core.CoefficientSign where

import qualified Deka.Native as DN
import qualified Deka.Dec as D

data T = T
  { coefficient :: DN.Coefficient
  , sign :: D.Sign
  } deriving (Eq, Ord, Show)
