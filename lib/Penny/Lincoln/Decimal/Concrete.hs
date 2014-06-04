{-# LANGUAGE OverloadedStrings #-}
-- | The 'Concrete' data type and associated functions.  These
-- are concrete numbers, together with a 'Lane' (that is, whether it
-- is a debit, credit, or zero.)  These numbers cannot be rendered
-- as strings; for that, you will need to convert them to a 'Rep'.
-- However, 'Concrete' types are the only ones with which you can
-- perform arithmetic.
module Penny.Lincoln.Decimal.Concrete where

import Penny.Lincoln.Decimal.Components
import Penny.Lincoln.Decimal.Normal hiding
  (negate, Add(..), Mult(..))
import qualified Penny.Lincoln.Decimal.Normal as N
import Penny.Lincoln.Equivalent
import Penny.Lincoln.Decimal.Lane
import Penny.Lincoln.Decimal.Side
import Prelude hiding (exponent, negate)
import Deka.Dec (Dec)
import Penny.Lincoln.Decimal.Abstract
import Data.Monoid

-- | Concrete representation of a number, together with a 'Lane'
-- (that is, whether it is a 'Debit', 'Credit', or neither.)  A
-- 'Concrete' cannot be rendered as a string; for that, use the
-- functions in "Penny.Lincoln.Decimal.Represent" to convert the
-- 'Concrete' to a 'Abstract' and then use
-- 'Penny.Lincoln.Decimal.render'.
--
-- 'Debit's are represented as positive signed numbers; 'Credit's as
-- negative numbers.
newtype Concrete = Concrete { unConcrete :: Normal }
  deriving (Eq, Ord, Show)

instance Equivalent Concrete where
  compareEv (Concrete x) (Concrete y) = compareEv x y

instance Laned Concrete where
  lane (Concrete d) = case signedDecuple d of
    Nothing -> Center
    Just (s, dc) -> NonCenter (sd, dc)
      where
        sd = case s of
          Pos -> Debit
          Neg -> Credit

instance HasExponent Concrete where
  exponent = exponent . unConcrete

instance HasCoefficient Concrete where
  coefficient = coefficient . unConcrete

-- | Things that can be converted to a concrete representation.
class HasConcrete a where
  concrete :: a -> Concrete

instance HasConcrete Concrete where
  concrete = id

instance HasConcrete Dec where
  concrete = Concrete . normal

instance HasConcrete Rep where
  concrete r = Concrete d
    where
      d = normal p
      p = Params sgn coe ex
      coe = coefficient r
      ex = exponent r
      sgn = case lane r of
        Center -> Sign0
        NonCenter (s, _) -> case s of
          Debit -> Sign0
          Credit -> Sign1

instance HasConcrete Abstract where
  concrete = concrete . absRep

-- | Ordinary addition.
add :: Concrete -> Concrete -> Concrete
add (Concrete x) (Concrete y) = Concrete $ x + y

-- | Ordinary subtraction.
subt :: Concrete -> Concrete -> Concrete
subt (Concrete x) (Concrete y) = Concrete $ x - y

-- | Ordinary multiplication.
mult :: Concrete -> Concrete -> Concrete
mult (Concrete x) (Concrete y) = Concrete $ x * y

-- | Flips 'Debit' to 'Credit' and vice versa; zeroes are unchanged.
negate :: Concrete -> Concrete
negate (Concrete x) = Concrete $ N.negate x

-- | Monoid under addition
newtype Add = Add { unAdd :: Concrete }
  deriving (Eq, Ord, Show)

instance Monoid Add where
  mempty = Add $ Concrete zero
  mappend (Add x) (Add y) = Add $ x `add` y

-- | Monoid under multiplication
newtype Mult = Mult { unMult :: Concrete }
  deriving (Eq, Ord, Show)

instance Monoid Mult where
  mempty = Mult $ Concrete one
  mappend (Mult x) (Mult y) = Mult $ x `mult` y
