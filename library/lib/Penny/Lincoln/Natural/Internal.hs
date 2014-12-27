module Penny.Lincoln.Natural.Internal where

import Penny.Lincoln.Digits
import Data.Foldable (Foldable, toList)

newtype NonZero = NonZero { nonZeroToInteger :: Integer }
  deriving (Eq, Ord, Show)

instance OneToNine NonZero where
  fromNovem = NonZero . digitToInt

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

instance Natural NonZero where
  next (NonZero i) = NonZero (succ i)
  prev (NonZero i)
    | i > 1 = Just $ NonZero (pred i)
    | otherwise = Nothing
  naturalToInteger (NonZero i) = i
  integerToNatural i
    | i < 1 = Nothing
    | otherwise = Just . NonZero $ i
  add (NonZero x) (NonZero y) = NonZero $ x + y
  mult (NonZero x) (NonZero y) = NonZero $ x * y

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

addNonZeroToUnsigned :: Unsigned -> NonZero -> Unsigned
addNonZeroToUnsigned (Unsigned x) (NonZero y) = Unsigned $ x + y

monus :: Unsigned -> Unsigned -> Unsigned
monus (Unsigned x) (Unsigned y) = Unsigned . max 0 $ x - y

subt :: Unsigned -> Unsigned -> Maybe Unsigned
subt (Unsigned x) (Unsigned y)
  | y > x = Nothing
  | otherwise = Just . Unsigned $ x - y


pow :: Unsigned -> Unsigned -> Unsigned
pow (Unsigned x) (Unsigned y) = Unsigned $ x ^ y

divide :: Unsigned -> NonZero -> (Unsigned, Unsigned)
divide (Unsigned x) (NonZero y) = (Unsigned q, Unsigned r)
  where
    (q, r) = x `divMod` y

length :: Foldable f => f a -> Unsigned
length = Unsigned . fromIntegral . Prelude.length . toList

unsignedToNonZero :: Unsigned -> Maybe NonZero
unsignedToNonZero (Unsigned u)
  | u > 0 = Just . NonZero $ u
  | otherwise = Nothing

nonZeroToUnsigned :: NonZero -> Unsigned
nonZeroToUnsigned (NonZero n) = Unsigned n

addUnsignedToNonZero :: NonZero -> Unsigned -> NonZero
addUnsignedToNonZero (NonZero x) (Unsigned y) = NonZero $ x + y

