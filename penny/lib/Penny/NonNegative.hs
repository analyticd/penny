{-# LANGUAGE BangPatterns #-}
module Penny.NonNegative
  ( NonNegative
  , c'Integer'NonNegative
  , nonNegativeToIntegerWithSign
  , c'Positive'NonNegative
  , c'NonNegative'Positive
  , c'NonNegative'Integer
  , stripSign
  , length
  , next
  , prev
  , add
  , subt
  , mult
  , pow
  , Differ(LeftBiggerBy, RightBiggerBy, Equal)
  , diff

  , zero
  , one
  , two
  , three
  , four
  , five
  , six
  , seven
  , eight
  , nine
  , ten
  ) where

import Data.Sequence (Seq, viewl, ViewL(EmptyL, (:<)))
import Penny.NonNegative.Internal
import Prelude hiding (length)
import Penny.Positive.Internal
import Penny.Polar

length :: Seq a -> NonNegative
length = go zero
  where
    go !acc sq = case viewl sq of
      EmptyL -> acc
      _ :< xs -> go (next acc) xs

zero, one, two, three, four, five, six, seven, eight, nine, ten :: NonNegative

zero = NonNegative 0
one = NonNegative 1
two = NonNegative 2
three = NonNegative 3
four = NonNegative 4
five = NonNegative 5
six = NonNegative 6
seven = NonNegative 7
eight = NonNegative 8
nine = NonNegative 9
ten = NonNegative 10

add :: NonNegative -> NonNegative -> NonNegative
add (NonNegative a) (NonNegative b) = NonNegative $ a + b

subt :: NonNegative -> NonNegative -> Maybe NonNegative
subt (NonNegative l) (NonNegative r)
  | r > l = Nothing
  | otherwise = Just . NonNegative $ l - r

mult :: NonNegative -> NonNegative -> NonNegative
mult (NonNegative x) (NonNegative y)
  = NonNegative $ x * y

next :: NonNegative -> NonNegative
next (NonNegative a) = NonNegative $ a + 1

prev :: NonNegative -> Maybe NonNegative
prev (NonNegative x)
  | x > 0 = Just $ NonNegative $ x - 1
  | otherwise = Nothing

c'NonNegative'Integer :: Integer -> Maybe NonNegative
c'NonNegative'Integer i
  | i < 0 = Nothing
  | otherwise = Just . NonNegative $ i

c'NonNegative'Positive :: Positive -> NonNegative
c'NonNegative'Positive = NonNegative . c'Integer'Positive

c'Positive'NonNegative :: NonNegative -> Maybe Positive
c'Positive'NonNegative (NonNegative i)
  | i < 1 = Nothing
  | otherwise = Just . Positive $ i

nonNegativeToIntegerWithSign :: Pole -> NonNegative -> Integer
nonNegativeToIntegerWithSign p (NonNegative i)
  | p == negative = negate i
  | otherwise = i

stripSign :: Integer -> NonNegative
stripSign = NonNegative . abs

pow :: NonNegative -> NonNegative -> NonNegative
pow (NonNegative b) (NonNegative e) = NonNegative $ b ^ e

data Differ a
  = LeftBiggerBy a
  | RightBiggerBy a
  | Equal
  deriving (Eq, Ord, Show)

diff :: NonNegative -> NonNegative -> Differ Positive
diff (NonNegative l) (NonNegative r)
  | l > r = LeftBiggerBy (Positive (l - r))
  | r > l = RightBiggerBy (Positive (r - l))
  | otherwise = Equal
