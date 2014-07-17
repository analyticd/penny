-- | Normal, signed, finite decimal numbers.

{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Penny.Lincoln.Decimal.Normal
  ( -- * Normal numbers
    Normal
  , unNormal
  , simpleCompare
  , simpleEq

  -- * Conversions
  , D.Sign(..)
  , D.PosNeg(..)
  , signedDecuple
  , Params(..)
  , params
  , normal
  , decToNormal

  -- * Arithmetic
  -- | 'Normal' is also an instance of 'Num', so you can perform
  -- ordinary arithmetic on it and convert it using 'fromInteger'.
  , negate
  , Penny.Lincoln.Decimal.Normal.isZero

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
import qualified Deka.Native.Abstract as DN
import Control.Exception
import Penny.Lincoln.Equivalent
import Penny.Lincoln.Decimal.Components
import Penny.Lincoln.Natural
import qualified Data.ByteString.Char8 as BS8
import Data.Monoid
import Prelude hiding (negate, exponent)
import qualified Prelude

-- | A 'Normal' wraps a 'Deka.Dec.Dec'.  It is possible for
-- arithmetic operations to exceed the available limits of the Deka
-- library; in this case, 'ArithmeticError' is thrown.
newtype ArithmeticError =
  ArithmeticError { unArithmeticError :: String }
  deriving (Eq, Ord, Show, Typeable)

instance Exception ArithmeticError

compute :: D.Ctx a -> a
compute c
  | fl == D.emptyFlags = r
  | otherwise = throw $ ArithmeticError "computation out of range"
  where
    (r, fl) = D.runCtxStatus c

-- | A normal, signed, finite decimal number.  Like 'Deka.Dec.Dec',
-- it has a coefficient and an exponent; however, the exponent is
-- always less than or equal to zero.
newtype Normal = Normal { unNormal :: D.Dec }
  deriving Show

-- | Larger numbers are greater than smaller numbers and, for
-- example, @1.00000@ is less than @1.0@.
instance Ord Normal where
  compare (Normal x) (Normal y) = D.compareTotal x y

-- | Numbers with the same coefficient but different exponents are
-- not equivalent; for example, @1.00000@ is not equal to @1.0@.
instance Eq Normal where
  x == y = compare x y == EQ

-- | If you use 'compare' on two 'Normal', the comparison is based
-- upon a total ordering so that, for example, @3.5@ is greater than
-- @3.5000@.  'simpleCompare' compares so that @3.5@ is equal to
-- @3.5000@.

simpleCompare :: Normal -> Normal -> Ordering
simpleCompare (Normal x) (Normal y) = compute $ do
  r <- D.compare x y
  return $ case () of
    _ | D.isZero r -> EQ
      | D.isPositive r -> GT
      | otherwise -> LT

-- | Like 'simpleCompare' but for equality.

simpleEq :: Normal -> Normal -> Bool
simpleEq x y = simpleCompare x y == EQ

-- | Uses 'simpleCompare' and 'simpleEq'.

instance Equivalent Normal where
  equivalent = simpleEq
  compareEv = simpleCompare

instance HasExponent Normal where
  exponent (Normal d) = case DN.value a of
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

instance HasCoefficient Normal where
  coefficient (Normal d) = case DN.value a of
    DN.Finite c _ -> c
    _ -> error "Concrete.HasCoefficient: invalid Dec"
    where
      a = DN.decToAbstract d

instance Signed Normal where
  sign (Normal d)
    | D.isSigned d = Sign1
    | otherwise = Sign0

-- | Three parameters that define any Normal number.
data Params = Params
  { pmSign :: D.Sign
  , pmCoefficient :: DN.Coefficient
  , pmExponent :: Exponent
  } deriving (Eq, Ord, Show)

instance HasCoefficient Params where
  coefficient = pmCoefficient

instance HasExponent Params where
  exponent = pmExponent

instance Signed Params where
  sign = pmSign

params :: Normal -> Params
params (Normal d) = Params sgn coe ex
  where
    DN.Abstract sgn val = DN.decToAbstract d
    (coe, ex) = case val of
      DN.Finite c e -> (c, expnt)
        where
          expnt = Exponent
            . maybe (error "params: positive exponent") id
            . nonNegative
            . Prelude.negate
            . DN.firmadoToInt
            . DN.unExponent $ e
      _ -> error "params: bad number value"

-- | Gets the 'D.PosNeg' and 'DN.Decuple' of a non-zero 'Normal';
-- returns 'Nothing' for a zero 'Normal'.
signedDecuple :: Normal -> Maybe (D.PosNeg, DN.Decuple)
signedDecuple n = case DN.unCoefficient . pmCoefficient $ pm of
  DN.Nil -> Nothing
  DN.Plenus dc -> Just (pn, dc)
  where
    pm = params n
    pn = case pmSign pm of
      D.Sign0 -> D.Pos
      D.Sign1 -> D.Neg

normal
  :: (HasCoefficient a, HasExponent a, Signed a)
  => a
  -> Normal
normal a = Normal d
  where
    abstract = DN.Abstract (sign a)
      $ DN.Finite (coefficient a) ex
    d | fl == D.emptyFlags = dec
      | otherwise = throw
          $ ArithmeticError "normal: value out of range"
    (dec, fl) = DN.abstractToDec abstract
    ex = DN.Exponent . DN.intToFirmado
      . Prelude.negate . unNonNegative
      . unExponent . exponent $ a


-- | Fails if the Dec is not normal.
decToNormal :: D.Dec -> Maybe Normal
decToNormal a
  | compute . D.isNormal $ a = Just $ Normal a
  | otherwise = Nothing

zero :: Normal
zero = Normal . compute $ D.fromByteString "0"

one :: Normal
one = Normal . compute $ D.fromByteString "1"

negate :: Normal -> Normal
negate = Normal . compute . D.minus . unNormal

isZero :: Normal -> Bool
isZero (Normal d) = D.isZero d

instance Num Normal where
  (Normal x) + (Normal y) = Normal . compute $ D.add x y
  (Normal x) * (Normal y) = Normal . compute $ D.multiply x y
  (Normal x) - (Normal y) = Normal . compute $ D.subtract x y
  abs (Normal x) = Normal . compute $ D.abs x
  signum (Normal x) = Normal . compute $ case () of
    _ | D.isNegative x -> D.fromByteString "-1"
      | D.isZero x -> D.fromByteString "0"
      | otherwise -> D.fromByteString "1"
  fromInteger = Normal . compute . D.fromByteString
    . BS8.pack . show

-- | Monoid under addition
newtype Add = Add { unAdd :: Normal }
  deriving (Eq, Ord, Show)

instance Monoid Add where
  mempty = Add zero
  mappend (Add x) (Add y) = Add $ x + y

-- | Monoid under multiplication
newtype Mult = Mult { unMult :: Normal }
  deriving (Eq, Ord, Show)

instance Monoid Mult where
  mempty = Mult one
  mappend (Mult x) (Mult y) = Mult $ x * y