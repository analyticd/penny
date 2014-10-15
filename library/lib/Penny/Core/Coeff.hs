module Penny.Core.Coeff where

import qualified Penny.Core.NovSign as NovSign

-- | Coefficients.  Unlike Deka coefficients, these carry the sign of
-- the number.

data T
  = Zero
  -- ^ All 'Zero' have a 'D.Sign' of 'D.Sign0'; that is, no
  -- negative zeroes are allowed.
  | NonZero NovSign.T
  deriving (Eq, Ord, Show)

