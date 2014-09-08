{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, BangPatterns #-}
-- | Concrete numbers.  Also has facilities to get the abstract
-- components of any concrete number, and to convert an abstract,
-- ungrouped number to a concrete number.

module Penny.Concrete where

{-
  ( -- * Concrete numbers
    Concrete
  , unConcrete
  , simpleCompare
  , simpleEq

  -- * Conversions
  , novDecsToDecuple
  , decupleToNovDecs
  , Coefficient(..)
  , Exponent(..)
  , Params(..)
  , params
  , concrete
  , decToConcrete

  -- * Arithmetic
  -- | 'Normal' is also an instance of 'Num', so you can perform
  -- ordinary arithmetic on it and convert it using 'fromInteger'.
  , negate
  , isZero

  -- * Constants
  , one
  , zero

  -- * Monoids
  , Add(..)
  , Mult(..)

  -- * Errors
  , ArithmeticError(..)
  ) where

import Data.Typeable
import qualified Deka.Dec as D
import qualified Deka.Native as DN
import Deka.Native.Abstract
  (Novem(..), Decem(..))
import Control.Exception
import qualified Data.ByteString.Char8 as BS8
import Data.Monoid
import Prelude hiding (negate, exponent)
import qualified Data.Sequence as S
import qualified Data.Foldable as Fdbl
import Penny.Numbers.Natural

compute :: D.Ctx a -> a
compute c
  | fl == D.emptyFlags = r
  | otherwise = throw $ ArithmeticError "computation out of range"
  where
    (r, fl) = D.runCtxStatus c

-- | A normal, signed, finite decimal number.  Like 'Deka.Dec.Dec', it
-- has a coefficient and an exponent; however, the exponent is always
-- less than or equal to zero.  No negative zeroes are allowed.
newtype Concrete = Concrete { unConcrete :: D.Dec }
  deriving Show

-- | Larger numbers are greater than smaller numbers and, for
-- example, @1.00000@ is less than @1.0@.
instance Ord Concrete where
  compare (Concrete x) (Concrete y) = D.compareTotal x y

-- | Numbers with the same coefficient but different exponents are
-- not equivalent; for example, @1.00000@ is not equal to @1.0@.
instance Eq Concrete where
  x == y = compare x y == EQ

-- | If you use 'compare' on two 'Concrete', the comparison is based
-- upon a total ordering so that, for example, @3.5@ is greater than
-- @3.5000@.  'simpleCompare' compares so that @3.5@ is equal to
-- @3.5000@.

simpleCompare :: Concrete -> Concrete -> Ordering
simpleCompare (Concrete x) (Concrete y) = compute $ do
  r <- D.compare x y
  return $ case () of
    _ | D.isZero r -> EQ
      | D.isPositive r -> GT
      | otherwise -> LT

-- | Like 'simpleCompare' but for equality.

simpleEq :: Concrete -> Concrete -> Bool
simpleEq x y = simpleCompare x y == EQ

-- In next function, note that D.isNormal will return False if the
-- number is zero.

-- | Fails if the Dec is not normal, or if it is the negative zero.
decToConcrete :: D.Dec -> Maybe Concrete
decToConcrete a
  | D.isSigned a && D.isZero a = Nothing
  | D.isZero a = Just $ Concrete a
  | compute . D.isNormal $ a = Just $ Concrete a
  | otherwise = Nothing

zero :: Concrete
zero = Concrete . compute $ D.fromByteString "0"

one :: Concrete
one = Concrete . compute $ D.fromByteString "1"

negate :: Concrete -> Concrete
negate = Concrete . compute . D.minus . unConcrete

isZero :: Concrete -> Bool
isZero (Concrete d) = D.isZero d

instance Num Concrete where
  (Concrete x) + (Concrete y) = Concrete . compute $ D.add x y
  (Concrete x) * (Concrete y) = Concrete . compute $ D.multiply x y
  (Concrete x) - (Concrete y) = Concrete . compute $ D.subtract x y
  abs (Concrete x) = Concrete . compute $ D.abs x
  signum (Concrete x) = Concrete . compute $ case () of
    _ | D.isNegative x -> D.fromByteString "-1"
      | D.isZero x -> D.fromByteString "0"
      | otherwise -> D.fromByteString "1"
  fromInteger = Concrete . compute . D.fromByteString
    . BS8.pack . show

params :: Concrete -> Params
params (Concrete d) = Params (dekaCoefficientToPenny sgn coe) ex
  where
    DN.Abstract sgn val = DN.decToAbstract d
    (coe, ex) = case val of
      DN.Finite c e -> (c, expnt)
        where
          expnt = case DN.unExponent e of
            DN.Cero -> ExpZero
            DN.Completo pn dc
              | pn == D.Pos -> error "params: positive exponent"
              | otherwise -> ExpNegative . decupleToNovDecs $ dc
      _ -> error "params: bad number value"

concrete :: Params -> Concrete
concrete a = Concrete d
  where
    abstract = DN.Abstract sgn fin
    (coe, sgn) = pennyCoefficientToDeka . pmCoefficient $ a
    fin = DN.Finite coe (DN.Exponent ex)
    ex = case pmExponent a of
      ExpZero -> DN.Cero
      ExpNegative nv -> DN.Completo D.Neg (novDecsToDecuple nv)
    d | fl == D.emptyFlags = dec
      | otherwise = throw
          $ ArithmeticError "concrete: value out of range"
    (dec, fl) = DN.abstractToDec abstract

-}
