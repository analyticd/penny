{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses,
    FlexibleInstances #-}
-- | The 'Multiplier' data type and associated functions.  A
-- 'Multiplier' is a concrete representation of a price.
module Penny.Lincoln.Decimal.Multiplier where

import Penny.Lincoln.Decimal.Components
import Penny.Lincoln.Decimal.Normal hiding
  (negate, Add(..), Mult(..))
import qualified Penny.Lincoln.Decimal.Normal as N
import Penny.Lincoln.Equivalent
import Penny.Lincoln.Decimal.Lane
import Prelude hiding (exponent, negate)
import Deka.Dec (Dec)
import Penny.Lincoln.Decimal.Abstract
import Data.Monoid


-- | Concrete representation of a price, together with a 'PosNeg'
-- indicating whether the price is positive or negative.
newtype Multiplier = Multiplier { unMultiplier :: Normal }
  deriving (Eq, Ord, Show)

instance Equivalent Multiplier where
  compareEv (Multiplier x) (Multiplier y) = compareEv x y

instance Laned Multiplier PosNeg where
  lane (Multiplier d) = case signedDecuple d of
    Nothing -> Center
    Just (s, dc) -> NonCenter (s, dc)

instance HasExponent Multiplier where
  exponent = exponent . unMultiplier

instance HasCoefficient Multiplier where
  coefficient = coefficient . unMultiplier

-- | Things that can be converted to a concrete representation.
class HasMultiplier a where
  multiplier :: a -> Multiplier

instance HasMultiplier Multiplier where
  multiplier = id

instance HasMultiplier Dec where
  multiplier = Multiplier . normal

instance HasMultiplier (Rep PosNeg) where
  multiplier r = Multiplier d
    where
      d = normal p
      p = Params sgn coe ex
      coe = coefficient r
      ex = exponent r
      sgn = case lane r of
        Center -> Sign0
        NonCenter (s, _) -> case s of
          Pos -> Sign0
          Neg -> Sign1

instance HasMultiplier (Abstract PosNeg) where
  multiplier = multiplier . absRep

-- | Ordinary addition.
add :: Multiplier -> Multiplier -> Multiplier
add (Multiplier x) (Multiplier y) = Multiplier $ x + y

-- | Ordinary subtraction.
subt :: Multiplier -> Multiplier -> Multiplier
subt (Multiplier x) (Multiplier y) = Multiplier $ x - y

-- | Ordinary multiplication.
mult :: Multiplier -> Multiplier -> Multiplier
mult (Multiplier x) (Multiplier y) = Multiplier $ x * y

-- | Flips 'Debit' to 'Credit' and vice versa; zeroes are unchanged.
negate :: Multiplier -> Multiplier
negate (Multiplier x) = Multiplier $ N.negate x

-- | Monoid under addition
newtype Add = Add { unAdd :: Multiplier }
  deriving (Eq, Ord, Show)

instance Monoid Add where
  mempty = Add $ Multiplier zero
  mappend (Add x) (Add y) = Add $ x `add` y

-- | Monoid under multiplication
newtype Mult = Mult { unMult :: Multiplier }
  deriving (Eq, Ord, Show)

instance Monoid Mult where
  mempty = Mult $ Multiplier one
  mappend (Mult x) (Mult y) = Mult $ x `mult` y
