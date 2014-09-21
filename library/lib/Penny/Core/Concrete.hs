{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, BangPatterns #-}
-- | Concrete numbers.  Also has facilities to get the abstract
-- components of any concrete number, and to convert an abstract,
-- ungrouped number to a concrete number.

module Penny.Core.Concrete
  ( -- * Concrete numbers
    T
  , toDec
  , simpleCompare
  , simpleEq

  -- * Conversions
  , fromDec
  , toCement
  , fromCement

  -- * Arithmetic
  -- | 'T' is also an instance of 'Num', so you can perform
  -- ordinary arithmetic on it and convert it using 'fromInteger'.
  , negate
  , isZero

  -- * Constants
  , one
  , zero
  ) where

import qualified Deka.Dec as D
import qualified Deka.Native as DN
import Control.Exception
import qualified Data.ByteString.Char8 as BS8
import Prelude hiding (negate, exponent)
import qualified Penny.Core.ArithmeticError as Error
import qualified Penny.Core.Cement as Cement
import qualified Penny.Core.Coeff as Coeff
import qualified Penny.Core.Exp as Exp
import qualified Penny.Core.Anna.NovDecs as NovDecs
import qualified Penny.Core.CoefficientSign as CoeffSign

-- End Imports

compute :: D.Ctx a -> a
compute c
  | fl == D.emptyFlags = r
  | otherwise = throw $ Error.T "computation out of range"
  where
    (r, fl) = D.runCtxStatus c


-- | A normal, signed, finite decimal number.  Like 'Deka.Dec.Dec', it
-- has a coefficient and an exponent; however, the exponent is always
-- less than or equal to zero.  No negative zeroes are allowed.
newtype T = T { toDec :: D.Dec }
  deriving Show


-- | Larger numbers are greater than smaller numbers and, for
-- example, @1.00000@ is less than @1.0@.
instance Ord T where
  compare (T x) (T y) = D.compareTotal x y

-- | Numbers with the same coefficient but different exponents are
-- not equivalent; for example, @1.00000@ is not equal to @1.0@.
instance Eq T where
  x == y = compare x y == EQ

-- | If you use 'compare' on two 'Concrete', the comparison is based
-- upon a total ordering so that, for example, @3.5@ is greater than
-- @3.5000@.  'simpleCompare' compares so that @3.5@ is equal to
-- @3.5000@.

simpleCompare :: T -> T -> Ordering
simpleCompare (T x) (T y) = compute $ do
  r <- D.compare x y
  return $ case () of
    _ | D.isZero r -> EQ
      | D.isPositive r -> GT
      | otherwise -> LT

-- | Like 'simpleCompare' but for equality.

simpleEq :: T -> T -> Bool
simpleEq x y = simpleCompare x y == EQ

-- In next function, note that D.isNormal will return False if the
-- number is zero.

-- | Fails if the Dec is not normal, or if it is the negative zero.
fromDec :: D.Dec -> Maybe T
fromDec a
  | D.isSigned a && D.isZero a = Nothing
  | D.isZero a = Just $ T a
  | compute . D.isNormal $ a = Just $ T a
  | otherwise = Nothing


zero :: T
zero = T . compute $ D.fromByteString "0"

one :: T
one = T . compute $ D.fromByteString "1"

negate :: T -> T
negate = T . compute . D.minus . toDec

isZero :: T -> Bool
isZero (T d) = D.isZero d

-- | Results from opeations in 'Num' may be out of range; if this
-- happens, the function will throw 'Error.T'.
instance Num T where
  (T x) + (T y) = T . compute $ D.add x y
  (T x) * (T y) = T . compute $ D.multiply x y
  (T x) - (T y) = T . compute $ D.subtract x y
  abs (T x) = T . compute $ D.abs x
  signum (T x) = T . compute $ case () of
    _ | D.isNegative x -> D.fromByteString "-1"
      | D.isZero x -> D.fromByteString "0"
      | otherwise -> D.fromByteString "1"
  fromInteger = T . compute . D.fromByteString
    . BS8.pack . show


toCement :: T -> Cement.T
toCement (T d) = Cement.T (Coeff.fromCoefficientSign ce) ex
  where
    DN.Abstract sgn val = DN.decToAbstract d
    ce = CoeffSign.T coe sgn
    (coe, ex) = case val of
      DN.Finite c e -> (c, expnt)
        where
          expnt = case DN.unExponent e of
            DN.Cero -> Exp.Zero
            DN.Completo pn dc
              | pn == D.Pos -> error "params: positive exponent"
              | otherwise -> Exp.Negative . NovDecs.fromDecuple $ dc
      _ -> error "params: bad number value"

fromCement :: Cement.T -> T
fromCement a = T d
  where
    abstract = DN.Abstract sgn fin
    CoeffSign.T coe sgn = Coeff.toCoefficientSign . Cement.coefficient $ a
    fin = DN.Finite coe (DN.Exponent ex)
    ex = case Cement.exponent a of
      Exp.Zero -> DN.Cero
      Exp.Negative nv -> DN.Completo D.Neg (NovDecs.toDecuple nv)
    d | fl == D.emptyFlags = dec
      | otherwise = throw
          $ Error.T "concrete: value out of range"
    (dec, fl) = DN.abstractToDec abstract

