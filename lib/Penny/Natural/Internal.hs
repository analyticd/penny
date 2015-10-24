{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK not-home #-}
module Penny.Natural.Internal where

import Penny.Digit
import Penny.Grammar
  (Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine,
   D2, D3, D4, D5, D6, D7, D8, D9,
   D1z, D2z, D3z, D4z, D5z, D6z, D7z, D8z, D9z)
import Data.Foldable (toList)
import Data.Sequence (Seq, ViewR(..), viewr)
import Penny.Polar

-- Do not try to make 'Positive' an instance of 'Wrapped' in Lens.
-- That would allow the user to make a 'Positive' with any 'Integer',
-- which would break type safety.

newtype Positive = Positive { positiveToInteger :: Integer }
  deriving (Eq, Ord, Show)

class IsPositive a where
  toPositive :: a -> Positive

instance IsPositive One where toPositive = Positive . digitToInt
instance IsPositive Two where toPositive = Positive . digitToInt
instance IsPositive Three where toPositive = Positive . digitToInt
instance IsPositive Four where toPositive = Positive . digitToInt
instance IsPositive Five where toPositive = Positive . digitToInt
instance IsPositive Six where toPositive = Positive . digitToInt
instance IsPositive Seven where toPositive = Positive . digitToInt
instance IsPositive Eight where toPositive = Positive . digitToInt
instance IsPositive Nine where toPositive = Positive . digitToInt

instance IsPositive D2 where toPositive = Positive . digitToInt
instance IsPositive D3 where toPositive = Positive . digitToInt
instance IsPositive D4 where toPositive = Positive . digitToInt
instance IsPositive D5 where toPositive = Positive . digitToInt
instance IsPositive D6 where toPositive = Positive . digitToInt
instance IsPositive D7 where toPositive = Positive . digitToInt
instance IsPositive D8 where toPositive = Positive . digitToInt
instance IsPositive D9 where toPositive = Positive . digitToInt

-- Do not try to make 'Unsigned' an instance of 'Wrapped' in Lens.
-- That would allow the user to make a 'Unsigned' with any 'Integer',
-- which would break type safety.

-- | An integral value that is at least zero.
newtype Unsigned = Unsigned { unsignedToInteger :: Integer }
  deriving (Eq, Ord, Show)

class IsUnsigned a where
  toUnsigned :: a -> Unsigned

instance IsUnsigned Zero where toUnsigned = Unsigned . digitToInt
instance IsUnsigned D1z where toUnsigned = Unsigned . digitToInt
instance IsUnsigned D2 where toUnsigned = Unsigned . digitToInt
instance IsUnsigned D2z where toUnsigned = Unsigned . digitToInt
instance IsUnsigned D3 where toUnsigned = Unsigned . digitToInt
instance IsUnsigned D3z where toUnsigned = Unsigned . digitToInt
instance IsUnsigned D4 where toUnsigned = Unsigned . digitToInt
instance IsUnsigned D4z where toUnsigned = Unsigned . digitToInt
instance IsUnsigned D5 where toUnsigned = Unsigned . digitToInt
instance IsUnsigned D5z where toUnsigned = Unsigned . digitToInt
instance IsUnsigned D6 where toUnsigned = Unsigned . digitToInt
instance IsUnsigned D6z where toUnsigned = Unsigned . digitToInt
instance IsUnsigned D7 where toUnsigned = Unsigned . digitToInt
instance IsUnsigned D7z where toUnsigned = Unsigned . digitToInt
instance IsUnsigned D8 where toUnsigned = Unsigned . digitToInt
instance IsUnsigned D8z where toUnsigned = Unsigned . digitToInt
instance IsUnsigned D9 where toUnsigned = Unsigned . digitToInt
instance IsUnsigned D9z where toUnsigned = Unsigned . digitToInt

class Natural a where
  next :: a -> a
  prev :: a -> Maybe a
  naturalToInteger :: a -> Integer
  integerToNatural :: Integer -> Maybe a
  add :: a -> a -> a
  mult :: a -> a -> a

instance Natural Positive where
  next (Positive i) = Positive (succ i)
  prev (Positive i)
    | i > 1 = Just $ Positive (pred i)
    | otherwise = Nothing
  naturalToInteger (Positive i) = i
  integerToNatural i
    | i < 1 = Nothing
    | otherwise = Just . Positive $ i
  add (Positive x) (Positive y) = Positive $ x + y
  mult (Positive x) (Positive y) = Positive $ x * y

instance Natural Unsigned where
  next (Unsigned i) = Unsigned (succ i)
  prev (Unsigned i)
    | i > 0 = Just . Unsigned . pred $ i
    | otherwise = Nothing
  naturalToInteger (Unsigned i) = i
  integerToNatural i
    | i < 0 = Nothing
    | otherwise = Just . Unsigned $ i
  add (Unsigned x) (Unsigned y) = Unsigned $ x + y
  mult (Unsigned x) (Unsigned y) = Unsigned $ x * y

instance Natural Integer where
  next = succ
  prev i
    | i > 0 = Just . pred $ i
    | otherwise = Nothing
  naturalToInteger = id
  integerToNatural i
    | i < 0 = Nothing
    | otherwise = Just i
  add = (+)
  mult = (*)

addPositiveToUnsigned :: Unsigned -> Positive -> Unsigned
addPositiveToUnsigned (Unsigned x) (Positive y) = Unsigned $ x + y

monus :: Unsigned -> Unsigned -> Unsigned
monus (Unsigned x) (Unsigned y) = Unsigned . max 0 $ x - y

subt :: Unsigned -> Unsigned -> Maybe Unsigned
subt (Unsigned x) (Unsigned y)
  | y > x = Nothing
  | otherwise = Just . Unsigned $ x - y

-- | Converts an 'Integer' to an 'Unsigned' by stripping any negative
-- sign.

absolute :: Integer -> Unsigned
absolute i
  | i < 0 = Unsigned . negate $ i
  | otherwise = Unsigned i

class Pow a where
  -- | Take the original number and multiply it by ten raised to the
  -- power of the given exponent.
  raise :: a -> Unsigned -> a

instance Pow Unsigned where
  raise (Unsigned x) (Unsigned y) = Unsigned $ x * 10 ^ y

instance Pow Integer where
  raise i (Unsigned y) = i * 10 ^ y

instance Pow Positive where
  raise (Positive x) (Unsigned y) = Positive $ x * 10 ^ y

divide :: Unsigned -> Positive -> (Unsigned, Unsigned)
divide (Unsigned x) (Positive y) = (Unsigned q, Unsigned r)
  where
    (q, r) = x `divMod` y

lengthUnsigned :: Foldable f => f a -> Unsigned
lengthUnsigned = Unsigned . fromIntegral . Prelude.length . toList

unsignedToPositive :: Unsigned -> Maybe Positive
unsignedToPositive (Unsigned u)
  | u > 0 = Just . Positive $ u
  | otherwise = Nothing

positiveToUnsigned :: Positive -> Unsigned
positiveToUnsigned (Positive n) = Unsigned n

addUnsignedToPositive :: Positive -> Unsigned -> Positive
addUnsignedToPositive (Positive x) (Unsigned y) = Positive $ x + y

novDecsToPositive :: D9 -> Seq D9z -> Positive
novDecsToPositive n = Positive . finish . go (0 :: Int) 0
  where
    go !places !tot sq = case viewr sq of
      EmptyR -> (places, tot)
      xs :> x -> go (succ places) ((digitToInt x * 10 ^ places) + tot) xs
    finish (places, tot) = (digitToInt n * 10 ^ places) + tot

-- | Strips the sign from an Integer and returns a Positive paired up
-- with the sign.  Returns Nothing if the Integer is zero.
stripIntegerSign :: Integer -> Maybe (Positive, Pole)
stripIntegerSign i
  | i == 0 = Nothing
  | i < 0 = Just (Positive (negate i), negative)
  | otherwise = Just (Positive i, positive)

-- | Transform a 'Positive' into its component digits.
positiveDigits
  :: Positive
  -> (D9, [D9z])
positiveDigits (Positive i) = go i []
  where
    go leftOver acc
      | quotient == 0 = (lastDigit, acc)
      | otherwise = go quotient (thisDigit : acc)
      where
        (quotient, remainder) = leftOver `divMod` 10
        thisDigit = case intToDigit remainder of
          Just d -> d
          Nothing -> error "positiveDigits: error 1"
        lastDigit = case intToDigit remainder of
          Just d -> d
          Nothing -> error "positiveDigits: error 2"

data Differ a
  = LeftBiggerBy a
  | RightBiggerBy a
  | Equal
  deriving (Eq, Ord, Show)

diffUnsigned :: Unsigned -> Unsigned -> Differ Positive
diffUnsigned (Unsigned l) (Unsigned r)
  | l > r = LeftBiggerBy (Positive (l - r))
  | r > l = RightBiggerBy (Positive (r - l))
  | otherwise = Equal

