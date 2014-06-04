{-# LANGUAGE OverloadedStrings #-}
-- | The 'Concrete' data type and associated functions.  These
-- are concrete numbers, together with a 'Lane' (that is, whether it
-- is a debit, credit, or zero.)  These numbers cannot be rendered
-- as strings; for that, you will need to convert them to a 'Rep'.
-- However, 'Concrete' types are the only ones with which you can
-- perform arithmetic.
module Penny.Lincoln.Decimal.Concrete
  ( 
  -- * The 'Concrete' type 
    Concrete
  , unConcrete
  , simpleCompare
  , simpleEq

  -- * Conversions
  , HasConcrete(..)

  -- * Arithmetic
  , add
  , subt
  , mult
  , negate

  -- * Monoids
  , Add(..)
  , Mult(..)

  -- * Constants
  , zero
  , one
  ) where

import Penny.Lincoln.Decimal.Abstract
import qualified Deka.Native.Abstract as DN
import qualified Deka.Native as DN
import Penny.Lincoln.Decimal.Components
import Penny.Lincoln.Natural
import qualified Deka.Dec as D
import Penny.Lincoln.Decimal.Lane
import Penny.Lincoln.Decimal.Side
import Prelude hiding (negate, exponent)
import qualified Prelude
import Data.Monoid(Monoid(..))
import Penny.Lincoln.Equivalent

-- | Concrete representation of a number, together with a 'Lane'
-- (that is, wehther it is a 'Debit', 'Credit', or neither.)  A
-- 'Concrete' cannot be rendered as a string; for that, use the
-- functions in "Penny.Lincoln.Decimal.Represent" to convert the
-- 'Concrete' to a 'Abstract' and then use
-- 'Penny.Lincoln.Decimal.render'.
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

-- | Uses 'simpleCompare' and 'simpleEq'.

instance Equivalent Concrete where
  equivalent = simpleEq
  compareEv = simpleCompare

instance Laned Concrete where
  lane (Concrete d)
    | D.isZero d = Center
    | D.isPositive d = NonCenter (Debit, dc)
    | otherwise = NonCenter (Credit, dc)
    where
      dc = case DN.value . DN.decToAbstract $ d of
        DN.Finite c _ -> case DN.unCoefficient c of
          DN.Plenus dcple -> dcple
          _ -> error "Concrete.Laned: invalid decoded"
        _ -> error "Concrete.Laned: impossible number type"

instance HasExponent Concrete where
  exponent (Concrete d) = case DN.value a of
    DN.Finite _ e -> case DN.unExponent e of
      DN.Cero -> Exponent . maybe (error "Concrete.HasExponent: error 1")
        id . nonNegative $ 0

      DN.Completo s dc -> case s of
        D.Pos -> error "Concrete.HasExponent: impossible sign"
        D.Neg -> Exponent
          . maybe (error "Concrete.HasExponent: error 2") id
          . nonNegative
          . DN.decupleToInt
          $ dc
    _ -> error "Concrete.HasExponent: invalid Dec"
    where
      a = DN.decToAbstract d

instance HasCoefficient Concrete where
  coefficient (Concrete d) = case DN.value a of
    DN.Finite c _ -> c
    _ -> error "Concrete.HasCoefficient: invalid Dec"
    where
      a = DN.decToAbstract d

-- | Things that can be converted to a concrete representation.
class HasConcrete a where
  concrete :: a -> Concrete

instance HasConcrete Concrete where
  concrete = id

instance HasConcrete D.Dec where
  concrete a
    | finite = Concrete a
    | otherwise = error "decToConcrete: not a normal number"
    where
      finite = compute . D.isNormal $ a

compute :: D.Ctx a -> a
compute c
  | fl == D.emptyFlags = r
  | otherwise = error
        "Penny.Lincoln.Decimal.Concrete: computation out of range"
  where
    (r, fl) = D.runCtxStatus c

instance HasConcrete Rep where
  concrete r = Concrete d
    where
      (sgn, aut) = case lane r of
        Center -> (D.Sign0, DN.Nil)
        NonCenter (sd, dc) -> (s, DN.Plenus dc)
          where
            s = case sd of { Debit -> D.Sign0; Credit -> D.Sign1 }
      ex = DN.Exponent . DN.intToFirmado
        . Prelude.negate . unNonNegative
        . unExponent . exponent $ r
      abstract = DN.Abstract sgn $ DN.Finite (DN.Coefficient aut) ex
      (dec, fl) = DN.abstractToDec abstract
      d | fl == D.emptyFlags = dec
        | otherwise = error "repToConcrete: value out of range"

instance HasConcrete Abstract where
  concrete = concrete . absRep

zero :: Concrete
zero = Concrete . compute $ D.fromByteString "0"

one :: Concrete
one = Concrete . compute $ D.fromByteString "1"

-- | Ordinary addition.
add :: Concrete -> Concrete -> Concrete
add (Concrete x) (Concrete y) = Concrete . compute $
  D.add x y

-- | Ordinary subtraction.
subt :: Concrete -> Concrete -> Concrete
subt (Concrete x) (Concrete y) = Concrete . compute $
  D.subtract x y

-- | Ordinary multiplication.
mult :: Concrete -> Concrete -> Concrete
mult (Concrete x) (Concrete y) = Concrete . compute $
  D.multiply x y

-- | Flips 'Debit' to 'Credit' and vice versa; zeroes are unchanged.
negate :: Concrete -> Concrete
negate (Concrete x) = Concrete . compute $ D.minus x

-- | Monoid under addition
newtype Add = Add { unAdd :: Concrete }
  deriving (Eq, Ord, Show)

instance Monoid Add where
  mempty = Add zero
  mappend (Add x) (Add y) = Add $ x `add` y

-- | Monoid under multiplication
newtype Mult = Mult { unMult :: Concrete }
  deriving (Eq, Ord, Show)

instance Monoid Mult where
  mempty = Mult one
  mappend (Mult x) (Mult y) = Mult $ x `mult` y
