{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK not-home #-}
module Penny.Lincoln.Natural.Internal where

import Penny.Lincoln.Rep.Digit
import Data.Foldable (Foldable, toList)
import Data.Sequence (Seq, ViewR(..), viewr)
import Penny.Lincoln.PluMin

newtype Positive = Positive { positiveToInteger :: Integer }
  deriving (Eq, Ord, Show)

class IsPositive a where
  toPositive :: a -> Positive

instance IsPositive D2 where toPositive = Positive . digitToInt
instance IsPositive D3 where toPositive = Positive . digitToInt
instance IsPositive D4 where toPositive = Positive . digitToInt
instance IsPositive D5 where toPositive = Positive . digitToInt
instance IsPositive D6 where toPositive = Positive . digitToInt
instance IsPositive D7 where toPositive = Positive . digitToInt
instance IsPositive D8 where toPositive = Positive . digitToInt
instance IsPositive D9 where toPositive = Positive . digitToInt

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

addPositiveToUnsigned :: Unsigned -> Positive -> Unsigned
addPositiveToUnsigned (Unsigned x) (Positive y) = Unsigned $ x + y

monus :: Unsigned -> Unsigned -> Unsigned
monus (Unsigned x) (Unsigned y) = Unsigned . max 0 $ x - y

subt :: Unsigned -> Unsigned -> Maybe Unsigned
subt (Unsigned x) (Unsigned y)
  | y > x = Nothing
  | otherwise = Just . Unsigned $ x - y


pow :: Unsigned -> Unsigned -> Unsigned
pow (Unsigned x) (Unsigned y) = Unsigned $ x ^ y

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
stripIntegerSign :: Integer -> Maybe (Positive, PluMin)
stripIntegerSign i
  | i == 0 = Nothing
  | i < 0 = Just (Positive (negate i), Minus)
  | otherwise = Just (Positive i, Plus)

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
