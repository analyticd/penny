module Penny.Core.Exp where

import qualified Penny.Natural.Unsigned as Unsigned

-- | Exponents.  Unlike exponents in Deka, Penny does not use
-- positive exponents because there is no unambiguous way to
-- represent them using ordinary notation.  All exponents are either
-- negative or zero.

newtype T = T { toUnsigned :: Unsigned.T }
  deriving (Eq, Ord, Show)

add :: T -> T -> T
add (T x) (T y) = T (Unsigned.add x y)

zero :: T
zero = T Unsigned.zero

fromUnsigned :: Unsigned.T -> T
fromUnsigned = T
