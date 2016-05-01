module Penny.Positive
  ( Positive
  , c'Integer'Positive
  , stripIntegerSign
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
  , add
  , mult
  , next
  , prev
  , pow
  ) where

import Penny.Positive.Internal
import Penny.NonNegative.Internal
import Penny.Polar

one, two, three, four, five, six, seven, eight, nine, ten :: Positive

one = Positive 1
two = Positive 2
three = Positive 3
four = Positive 4
five = Positive 5
six = Positive 6
seven = Positive 7
eight = Positive 8
nine = Positive 9
ten = Positive 10

add :: Positive -> Positive -> Positive
add (Positive a) (Positive b) = Positive $ a + b

mult :: Positive -> Positive -> Positive
mult (Positive a) (Positive b)
  = Positive $ a * b

next :: Positive -> Positive
next (Positive a) = Positive (a + 1)

prev :: Positive -> Maybe Positive
prev (Positive x)
  | x > 1 = Just . Positive $ x - 1
  | otherwise = Nothing

pow :: Positive -> NonNegative -> Positive
pow (Positive b) (NonNegative e) = Positive $ b ^ e

stripIntegerSign :: Integer -> Maybe (Positive, Pole)
stripIntegerSign i
  | i == 0 = Nothing
  | i < 0 = Just (Positive (Prelude.negate i), negative)
  | otherwise = Just (Positive i, positive)
