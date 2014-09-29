module Penny.Core.Coeff where

import qualified Deka.Dec as D
import qualified Penny.Core.NovDecs as NovDecs
import qualified Deka.Native as DN
import qualified Penny.Core.NovSign as NovSign
import qualified Penny.Core.CoefficientSign as CS

-- | Coefficients.  Unlike Deka coefficients, these carry the sign of
-- the number.

data T
  = Zero
  -- ^ All 'Zero' have a 'D.Sign' of 'D.Sign0'; that is, no
  -- negative zeroes are allowed.
  | NonZero NovSign.T
  deriving (Eq, Ord, Show)

fromCoefficientSign :: CS.T -> T
fromCoefficientSign (CS.T c s) = case DN.unCoefficient c of
  DN.Nil -> Zero
  DN.Plenus dc -> NonZero (NovSign.T (NovDecs.fromDecuple dc) s)

toCoefficientSign :: T -> CS.T
toCoefficientSign c = case c of
  Zero -> CS.T (DN.Coefficient DN.Nil) D.Sign0
  NonZero (NovSign.T nv sg) ->
    CS.T (DN.Coefficient $ DN.Plenus (NovDecs.toDecuple nv)) sg
