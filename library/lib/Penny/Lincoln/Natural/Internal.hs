module Penny.Lincoln.Natural.Internal where

import Penny.Lincoln.Digits
import Data.Foldable (Foldable, toList)

newtype Positive = Positive { positiveToInteger :: Integer }
  deriving (Eq, Ord, Show)

instance OneToNine Positive where
  fromNovem = Positive . digitToInt

newtype Unsigned = Unsigned { unsignedToInteger :: Integer }
  deriving (Eq, Ord, Show)

instance OneToNine Unsigned where
  fromNovem = Unsigned . digitToInt

instance Zero Unsigned where
  fromDecem = Unsigned . digitToInt

class Natural a where
  next :: a -> a
  prev :: a -> Maybe a
  naturalToInteger :: a -> Integer
  integerToNatural :: Integer -> Maybe a
  add :: a -> a -> a
  mult :: a -> a -> a

ten :: (OneToNine a, Natural a) => a
ten = fromNovem D1 `add` fromNovem D9

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

length :: Foldable f => f a -> Unsigned
length = Unsigned . fromIntegral . Prelude.length . toList

unsignedToPositive :: Unsigned -> Maybe Positive
unsignedToPositive (Unsigned u)
  | u > 0 = Just . Positive $ u
  | otherwise = Nothing

positiveToUnsigned :: Positive -> Unsigned
positiveToUnsigned (Positive n) = Unsigned n

addUnsignedToPositive :: Positive -> Unsigned -> Positive
addUnsignedToPositive (Positive x) (Unsigned y) = Positive $ x + y

