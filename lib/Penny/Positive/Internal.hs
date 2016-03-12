module Penny.Positive.Internal where

-- Do not try to make 'Positive' an instance of 'Wrapped' in Lens.
-- That would allow the user to make a 'Positive' with any 'Integer',
-- which would break type safety.

newtype Positive = Positive { c'Integer'Positive :: Integer }
  deriving (Eq, Ord, Show)

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

next :: Positive -> Positive
next (Positive a) = Positive (a + 1)
