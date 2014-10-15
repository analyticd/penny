module Penny.Core.Exp where

import qualified Penny.Core.NovDecs as NovDecs
import qualified Penny.Natural.Unsigned as Unsigned
import qualified Penny.Natural.NonZero as NonZero
import Deka.Native.Abstract

-- | Exponents.  Unlike exponents in Deka, Penny does not use
-- positive exponents because there is no unambiguous way to
-- represent them using ordinary notation.  All exponents are either
-- negative or zero.

data T
  = Zero
  | Negative NovDecs.T
  deriving (Eq, Ord, Show)

fromUnsigned :: Unsigned.T -> T
fromUnsigned u = case NonZero.fromUnsigned u of
  Nothing -> Zero
  Just nz -> Negative . NovDecs.fromNonZero $ nz
