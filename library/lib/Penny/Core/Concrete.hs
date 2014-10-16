{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, BangPatterns, NoImplicitPrelude #-}
-- | Concrete numbers.  Also has facilities to get the abstract
-- components of any concrete number, and to convert an abstract,
-- ungrouped number to a concrete number.

module Penny.Core.Concrete where

import Prelude hiding
  ( exponent
  , compare
  , abs
  , signum
  , fromInteger
  )
import qualified Prelude

import qualified Penny.Natural.Unsigned as Unsigned

data T = T
  { coefficient :: Integer
  , exponent :: Unsigned.T
  } deriving (Eq, Ord, Show)

isZero :: T -> Bool
isZero (T c _) = c == 0

-- | Compares two 'T' after equalizing their exponents.
compareEquiv :: T -> T -> Ordering
compareEquiv x y = Prelude.compare (coefficient x') (coefficient y')
  where
    (x', y') = equalizeExponents x y

-- | Decreases one of the two exponents (that is, makes it more
-- negative) as needed, while adjusting its coefficient accordingly,
-- so that the two numbers have equal exponents.
equalizeExponents :: T -> T -> (T, T)
equalizeExponents x y
  | exponent x > exponent y = (x, decreaseExponent (exponent x) y)
  | otherwise = (decreaseExponent (exponent y) x, y)

-- | Decreases the exponent (that is, makes it more negative) as
-- needed, while adjusting its coefficient accordingly, so that its
-- exponent has the given value.  Does nothing if the exponent is
-- already less than or equal to the given value.
decreaseExponent
  :: Unsigned.T
  -- ^ Decrease to this exponent
  -> T
  -> T
decreaseExponent tgt (T coe ex) = case Unsigned.subt tgt ex of
  Nothing -> T coe ex
  Just diff -> T (coe * 10 ^ (Unsigned.toInteger diff)) tgt

add :: T -> T -> T
add x y = T (cx + cy) ex
  where
    (T cx ex, T cy _) = equalizeExponents x y

subt :: T -> T -> T
subt x y = T (cx - cy) ex
  where
    (T cx ex, T cy _) = equalizeExponents x y

mult :: T -> T -> T
mult (T cx ex) (T cy ey) = T (cx * cy) (Unsigned.add ex ey)

abs :: T -> T
abs (T c e) = T (Prelude.abs c) e

signum :: T -> T
signum (T c _) = T (Prelude.signum c) (Unsigned.zero)

fromInteger :: Integer -> T
fromInteger i = T i Unsigned.zero

instance Prelude.Num T where
  x + y = add x y
  x * y = mult x y
  x - y = subt x y
  abs = abs
  signum = signum
  fromInteger = fromInteger

