{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK not-home #-}
module Penny.Natural.Internal where

import Data.Foldable (toList)
import Data.Sequence (Seq, ViewR(..), viewr)

import Penny.Digit
import Penny.Grammar
import Penny.Polar

-- Do not try to make 'Positive' an instance of 'Wrapped' in Lens.
-- That would allow the user to make a 'Positive' with any 'Integer',
-- which would break type safety.

newtype Positive = Positive { positiveToInteger :: Integer }
  deriving (Eq, Ord, Show)

instance HasOne Positive where one = Positive 1
instance HasTwo Positive where two = Positive 2
instance HasThree Positive where three = Positive 3
instance HasFour Positive where four = Positive 4
instance HasFive Positive where five = Positive 5
instance HasSix Positive where six = Positive 6
instance HasSeven Positive where seven = Positive 7
instance HasEight Positive where eight = Positive 8
instance HasNine Positive where nine = Positive 9
instance HasTen Positive where ten = Positive 10

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

instance IsPositive D1'9 where toPositive = Positive . digitToInt

-- Do not try to make 'Unsigned' an instance of 'Wrapped' in Lens.
-- That would allow the user to make a 'Unsigned' with any 'Integer',
-- which would break type safety.

-- | An integral value that is at least zero.
newtype Unsigned = Unsigned { unsignedToInteger :: Integer }
  deriving (Eq, Ord, Show)

instance HasZero Unsigned where zero = Unsigned 0
instance HasOne Unsigned where one = Unsigned 1
instance HasTwo Unsigned where two = Unsigned 2
instance HasThree Unsigned where three = Unsigned 3
instance HasFour Unsigned where four = Unsigned 4
instance HasFive Unsigned where five = Unsigned 5
instance HasSix Unsigned where six = Unsigned 6
instance HasSeven Unsigned where seven = Unsigned 7
instance HasEight Unsigned where eight = Unsigned 8
instance HasNine Unsigned where nine = Unsigned 9
instance HasTen Unsigned where ten = Unsigned 10

class IsUnsigned a where
  toUnsigned :: a -> Unsigned

instance IsUnsigned Zero where toUnsigned = Unsigned . digitToInt
instance IsUnsigned One where toUnsigned = Unsigned . digitToInt
instance IsUnsigned Two where toUnsigned = Unsigned . digitToInt
instance IsUnsigned Three where toUnsigned = Unsigned . digitToInt
instance IsUnsigned Four where toUnsigned = Unsigned . digitToInt
instance IsUnsigned Five where toUnsigned = Unsigned . digitToInt
instance IsUnsigned Six where toUnsigned = Unsigned . digitToInt
instance IsUnsigned Seven where toUnsigned = Unsigned . digitToInt
instance IsUnsigned Eight where toUnsigned = Unsigned . digitToInt
instance IsUnsigned Nine where toUnsigned = Unsigned . digitToInt

instance IsUnsigned D0'9 where toUnsigned = Unsigned . digitToInt
instance IsUnsigned D1'9 where toUnsigned = Unsigned . digitToInt
instance IsUnsigned D0'1 where toUnsigned = Unsigned . digitToInt
instance IsUnsigned D0'2 where toUnsigned = Unsigned . digitToInt
instance IsUnsigned D0'3 where toUnsigned = Unsigned . digitToInt
instance IsUnsigned D0'5 where toUnsigned = Unsigned . digitToInt

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

novDecsToPositive :: D1'9 -> Seq D0'9 -> Positive
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
  -> (D1'9, [D0'9])
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
