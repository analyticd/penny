module Penny.NonNegative.Internal where

import Penny.Positive.Internal

newtype NonNegative = NonNegative { c'Integer'NonNegative :: Integer }
  deriving (Eq, Ord, Show)

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

next :: NonNegative -> NonNegative
next (NonNegative a) = NonNegative $ a + 1

prev :: NonNegative -> Maybe NonNegative
prev (NonNegative x)
  | x > 0 = Just $ NonNegative $ x - 1
  | otherwise = Nothing

c'NonNegative'Positive :: Positive -> NonNegative
c'NonNegative'Positive = NonNegative . c'Integer'Positive

c'Positive'NonNegative :: NonNegative -> Maybe Positive
c'Positive'NonNegative (NonNegative i)
  | i < 1 = Nothing
  | otherwise = Just . Positive $ i
