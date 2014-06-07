-- | Basic components of all numbers.
module Penny.Lincoln.Decimal.Components
  ( Exponent(..)
  , HasExponent(..)
  , HasCoefficient(..)
  , HasDecuple(..)
  , Lane(..)
  , Side(..)
  , Opposite(..)
  , PosNeg(..)
  , Sign(..)
  , Signed(..)
  , Wrapped(..)
  , MaybeWrapped(..)
  ) where

import Penny.Lincoln.Natural
import Deka.Native.Abstract hiding (Exponent(..), sign)
import Deka.Dec (PosNeg(..))
import Deka.Dec (Sign(..))
import Prelude hiding (exponent)

-- | Exponents.  Unlike exponents in Deka, Penny does not use
-- positive exponents because there is no unambiguous way to
-- represent them using ordinary notation.  All exponents are either
-- negative or zero.  'Exponent' holds all exponents as a
-- 'NonNegative', which represents the absolute value of the actual
-- exponent.
newtype Exponent = Exponent { unExponent :: NonNegative }
  deriving (Eq, Show, Ord)

class HasExponent a where
  exponent :: a -> Exponent

class HasCoefficient a where
  coefficient :: a -> Coefficient

class HasDecuple a where
  decuple :: a -> Decuple

-- | Represents whether something is 'Center' or is off to the side,
-- with an accompanying 'Decuple' for the non-zero coefficient.
data Lane a
  = Center

  | NonCenter (a, Decuple)
  -- ^ Anything that is not 'Center' must also have a
  -- non-zero coefficient, which is the 'Decuple'.

  deriving (Eq, Ord, Show)

instance MaybeWrapped Lane where
  maybeUnwrap a = case a of
    Center -> Nothing
    NonCenter (x, _) -> Just x

data Side
  = Debit
  | Credit
  deriving (Eq, Ord, Show)

class Opposite a where
  opposite :: a -> a

instance Opposite Side where
  opposite a = case a of
    Debit -> Credit
    Credit -> Debit

instance Opposite PosNeg where
  opposite a = case a of
    Pos -> Neg
    Neg -> Pos

class Signed a where
  sign :: a -> Sign

instance Signed PosNeg where
  sign a = case a of
    Pos -> Sign0
    Neg -> Sign1

instance Signed Side where
  sign a = case a of
    Debit -> Sign0
    Credit -> Sign1

class Wrapped c where
  unwrap :: c a -> a

class MaybeWrapped c where
  maybeUnwrap :: c a -> Maybe a
