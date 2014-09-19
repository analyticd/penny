module Penny.Lincoln.Exp where

import qualified Penny.Lincoln.Anna as NovDecs

-- | Exponents.  Unlike exponents in Deka, Penny does not use
-- positive exponents because there is no unambiguous way to
-- represent them using ordinary notation.  All exponents are either
-- negative or zero.

data T
  = Zero
  | Negative NovDecs.T
  deriving (Eq, Ord, Show)
