-- | Representing digits.  There are two different groups of types in
-- this module.  Each type in this module represents only a single
-- digit.  The types with cardinal number names (such as 'One', 'Two',
-- etc) represent a single digit, and the type itself has only one
-- well-defined vaue.  The types that begin with @D@ (such as 'D1z' or
-- 'D2') represent a single digit, but the type itself can have more
-- than one well-defined value.  The @D@ types that do not end in @z@
-- have values to represent any digit from 1 up to the given value;
-- for example, @D9@ represents any digit from 1 to 9.  The @D@ types
-- that end in @z@ are similar, but they can also represent the digit
-- 0.

module Penny.Digit where

import Penny.Display

-- * Typeclass for any digit

class Digit a where
  digitToInt :: Integral b => a -> b
  intToDigit :: Integral b => b -> Maybe a

-- * Types that represent a single digit

data Zero = Zero
  deriving (Eq, Ord, Show)

instance Digit Zero where
  digitToInt x = case x of
    Zero -> 0

  intToDigit x = case x of
    0 -> Just Zero
    _ -> Nothing

instance Display Zero where display _ = ('0':)

data One = One
  deriving (Eq, Ord, Show)

instance Digit One where
  digitToInt x = case x of
    One -> 1

  intToDigit x = case x of
    1 -> Just One
    _ -> Nothing

instance Display One where display _ = ('1':)

data Two = Two
  deriving (Eq, Ord, Show)

instance Display Two where display _ = ('2':)

instance Digit Two where
  digitToInt x = case x of
    Two -> 2

  intToDigit x = case x of
    2 -> Just Two
    _ -> Nothing

data Three = Three
  deriving (Eq, Ord, Show)

instance Display Three where display _ = ('3':)

instance Digit Three where
  digitToInt x = case x of
    Three -> 3

  intToDigit x = case x of
    3 -> Just Three
    _ -> Nothing

data Four = Four
  deriving (Eq, Ord, Show)

instance Display Four where display _ = ('4':)

instance Digit Four where
  digitToInt x = case x of
    Four -> 4

  intToDigit x = case x of
    4 -> Just Four
    _ -> Nothing

data Five = Five
  deriving (Eq, Ord, Show)

instance Display Five where display _ = ('5':)

instance Digit Five where
  digitToInt x = case x of
    Five -> 5

  intToDigit x = case x of
    5 -> Just Five
    _ -> Nothing

data Six = Six
  deriving (Eq, Ord, Show)

instance Display Six where display _ = ('6':)

instance Digit Six where
  digitToInt x = case x of
    Six -> 6

  intToDigit x = case x of
    6 -> Just Six
    _ -> Nothing

data Seven = Seven
  deriving (Eq, Ord, Show)

instance Display Seven where display _ = ('7':)

instance Digit Seven where
  digitToInt x = case x of
    Seven -> 7

  intToDigit x = case x of
    7 -> Just Seven
    _ -> Nothing

data Eight = Eight
  deriving (Eq, Ord, Show)

instance Display Eight where display _ = ('8':)

instance Digit Eight where
  digitToInt x = case x of
    Eight -> 8

  intToDigit x = case x of
    8 -> Just Eight
    _ -> Nothing

data Nine = Nine
  deriving (Eq, Ord, Show)

instance Display Nine where display _ = ('9':)

instance Digit Nine where
  digitToInt x = case x of
    Nine -> 9

  intToDigit x = case x of
    9 -> Just Nine
    _ -> Nothing


-- * Types that represent a range of digits

data D1z = D1z'0 | D1z'1
  deriving (Eq, Ord, Show)

instance Digit D1z where
  digitToInt x = case x of
    D1z'0 -> 0
    D1z'1 -> 1

  intToDigit x = case x of
    0 -> Just D1z'0
    1 -> Just D1z'1
    _ -> Nothing

instance Display D1z where display = (:) . c'Char'D1z

c'Char'D1z :: D1z -> Char
c'Char'D1z x = case x of
  D1z'0 -> '0'
  D1z'1 -> '1'

data D2z = D2z'0 | D2z'1 | D2z'2
  deriving (Eq, Ord, Show)

instance Digit D2z where
  digitToInt x = case x of
    D2z'0 -> 0
    D2z'1 -> 1
    D2z'2 -> 2

  intToDigit x = case x of
    0 -> Just D2z'0
    1 -> Just D2z'1
    2 -> Just D2z'2
    _ -> Nothing

instance Display D2z where display = (:) . c'Char'D2z

c'Char'D2z :: D2z -> Char
c'Char'D2z x = case x of
  D2z'0 -> '0'
  D2z'1 -> '1'
  D2z'2 -> '2'

data D2 = D2'1 | D2'2
  deriving (Eq, Ord, Show)

instance Digit D2 where
  digitToInt x = case x of
    D2'1 -> 1
    D2'2 -> 2

  intToDigit x = case x of
    1 -> Just D2'1
    2 -> Just D2'2
    _ -> Nothing

instance Display D2 where display = (:) . c'Char'D2

c'Char'D2 :: D2 -> Char
c'Char'D2 x = case x of
  D2'1 -> '1'
  D2'2 -> '2'

c'D2z'D2 :: D2 -> D2z
c'D2z'D2 x = case x of
  D2'1 -> D2z'1
  D2'2 -> D2z'2

c'D2'D2z :: D2z -> Maybe D2
c'D2'D2z x = case x of
  D2z'0 -> Nothing
  D2z'1 -> Just D2'1
  D2z'2 -> Just D2'2

data D3z = D3z'0 | D3z'1 | D3z'2 | D3z'3
  deriving (Eq, Ord, Show)

instance Digit D3z where
  digitToInt x = case x of
    D3z'0 -> 0
    D3z'1 -> 1
    D3z'2 -> 2
    D3z'3 -> 3

  intToDigit x = case x of
    0 -> Just D3z'0
    1 -> Just D3z'1
    2 -> Just D3z'2
    3 -> Just D3z'3
    _ -> Nothing

instance Display D3z where display = (:) . c'Char'D3z

c'Char'D3z :: D3z -> Char
c'Char'D3z x = case x of
  D3z'0 -> '0'
  D3z'1 -> '1'
  D3z'2 -> '2'
  D3z'3 -> '3'

data D3 = D3'1 | D3'2 | D3'3
  deriving (Eq, Ord, Show)

instance Digit D3 where
  digitToInt x = case x of
    D3'1 -> 1
    D3'2 -> 2
    D3'3 -> 3

  intToDigit x = case x of
    1 -> Just D3'1
    2 -> Just D3'2
    3 -> Just D3'3
    _ -> Nothing

instance Display D3 where display = (:) . c'Char'D3

c'Char'D3 :: D3 -> Char
c'Char'D3 x = case x of
  D3'1 -> '1'
  D3'2 -> '2'
  D3'3 -> '3'

c'D3z'D3 :: D3 -> D3z
c'D3z'D3 x = case x of
  D3'1 -> D3z'1
  D3'2 -> D3z'2
  D3'3 -> D3z'3

c'D3'D3z :: D3z -> Maybe D3
c'D3'D3z x = case x of
  D3z'0 -> Nothing
  D3z'1 -> Just D3'1
  D3z'2 -> Just D3'2
  D3z'3 -> Just D3'3

data D4z = D4z'0 | D4z'1 | D4z'2 | D4z'3 | D4z'4
  deriving (Eq, Ord, Show)

instance Digit D4z where
  digitToInt x = case x of
    D4z'0 -> 0
    D4z'1 -> 1
    D4z'2 -> 2
    D4z'3 -> 3
    D4z'4 -> 4

  intToDigit x = case x of
    0 -> Just D4z'0
    1 -> Just D4z'1
    2 -> Just D4z'2
    3 -> Just D4z'3
    4 -> Just D4z'4
    _ -> Nothing

instance Display D4z where display = (:) . c'Char'D4z

c'Char'D4z :: D4z -> Char
c'Char'D4z x = case x of
  D4z'0 -> '0'
  D4z'1 -> '1'
  D4z'2 -> '2'
  D4z'3 -> '3'
  D4z'4 -> '4'

data D4 = D4'1 | D4'2 | D4'3 | D4'4
  deriving (Eq, Ord, Show)

instance Digit D4 where
  digitToInt x = case x of
    D4'1 -> 1
    D4'2 -> 2
    D4'3 -> 3
    D4'4 -> 4

  intToDigit x = case x of
    1 -> Just D4'1
    2 -> Just D4'2
    3 -> Just D4'3
    4 -> Just D4'4
    _ -> Nothing

instance Display D4 where display = (:) . c'Char'D4

c'Char'D4 :: D4 -> Char
c'Char'D4 x = case x of
  D4'1 -> '1'
  D4'2 -> '2'
  D4'3 -> '3'
  D4'4 -> '4'

c'D4z'D4 :: D4 -> D4z
c'D4z'D4 x = case x of
  D4'1 -> D4z'1
  D4'2 -> D4z'2
  D4'3 -> D4z'3
  D4'4 -> D4z'4

c'D4'D4z :: D4z -> Maybe D4
c'D4'D4z x = case x of
  D4z'0 -> Nothing
  D4z'1 -> Just D4'1
  D4z'2 -> Just D4'2
  D4z'3 -> Just D4'3
  D4z'4 -> Just D4'4

data D5z = D5z'0 | D5z'1 | D5z'2 | D5z'3 | D5z'4 | D5z'5
  deriving (Eq, Ord, Show)

instance Digit D5z where
  digitToInt x = case x of
    D5z'0 -> 0
    D5z'1 -> 1
    D5z'2 -> 2
    D5z'3 -> 3
    D5z'4 -> 4
    D5z'5 -> 5

  intToDigit x = case x of
    0 -> Just D5z'0
    1 -> Just D5z'1
    2 -> Just D5z'2
    3 -> Just D5z'3
    4 -> Just D5z'4
    5 -> Just D5z'5
    _ -> Nothing

instance Display D5z where display = (:) . c'Char'D5z

c'Char'D5z :: D5z -> Char
c'Char'D5z x = case x of
  D5z'0 -> '0'
  D5z'1 -> '1'
  D5z'2 -> '2'
  D5z'3 -> '3'
  D5z'4 -> '4'
  D5z'5 -> '5'

data D5 = D5'1 | D5'2 | D5'3 | D5'4 | D5'5
  deriving (Eq, Ord, Show)

instance Digit D5 where
  digitToInt x = case x of
    D5'1 -> 1
    D5'2 -> 2
    D5'3 -> 3
    D5'4 -> 4
    D5'5 -> 5

  intToDigit x = case x of
    1 -> Just D5'1
    2 -> Just D5'2
    3 -> Just D5'3
    4 -> Just D5'4
    5 -> Just D5'5
    _ -> Nothing

instance Display D5 where display = (:) . c'Char'D5

c'Char'D5 :: D5 -> Char
c'Char'D5 x = case x of
  D5'1 -> '1'
  D5'2 -> '2'
  D5'3 -> '3'
  D5'4 -> '4'
  D5'5 -> '5'

c'D5z'D5 :: D5 -> D5z
c'D5z'D5 x = case x of
  D5'1 -> D5z'1
  D5'2 -> D5z'2
  D5'3 -> D5z'3
  D5'4 -> D5z'4
  D5'5 -> D5z'5

c'D5'D5z :: D5z -> Maybe D5
c'D5'D5z x = case x of
  D5z'0 -> Nothing
  D5z'1 -> Just D5'1
  D5z'2 -> Just D5'2
  D5z'3 -> Just D5'3
  D5z'4 -> Just D5'4
  D5z'5 -> Just D5'5

data D6z = D6z'0 | D6z'1 | D6z'2 | D6z'3 | D6z'4 | D6z'5 | D6z'6
  deriving (Eq, Ord, Show)

instance Digit D6z where
  digitToInt x = case x of
    D6z'0 -> 0
    D6z'1 -> 1
    D6z'2 -> 2
    D6z'3 -> 3
    D6z'4 -> 4
    D6z'5 -> 5
    D6z'6 -> 6

  intToDigit x = case x of
    0 -> Just D6z'0
    1 -> Just D6z'1
    2 -> Just D6z'2
    3 -> Just D6z'3
    4 -> Just D6z'4
    5 -> Just D6z'5
    6 -> Just D6z'6
    _ -> Nothing

instance Display D6z where display = (:) . c'Char'D6z

c'Char'D6z :: D6z -> Char
c'Char'D6z x = case x of
  D6z'0 -> '0'
  D6z'1 -> '1'
  D6z'2 -> '2'
  D6z'3 -> '3'
  D6z'4 -> '4'
  D6z'5 -> '5'
  D6z'6 -> '6'

data D6 = D6'1 | D6'2 | D6'3 | D6'4 | D6'5 | D6'6
  deriving (Eq, Ord, Show)

instance Digit D6 where
  digitToInt x = case x of
    D6'1 -> 1
    D6'2 -> 2
    D6'3 -> 3
    D6'4 -> 4
    D6'5 -> 5
    D6'6 -> 6

  intToDigit x = case x of
    1 -> Just D6'1
    2 -> Just D6'2
    3 -> Just D6'3
    4 -> Just D6'4
    5 -> Just D6'5
    6 -> Just D6'6
    _ -> Nothing

instance Display D6 where display = (:) . c'Char'D6

c'Char'D6 :: D6 -> Char
c'Char'D6 x = case x of
  D6'1 -> '1'
  D6'2 -> '2'
  D6'3 -> '3'
  D6'4 -> '4'
  D6'5 -> '5'
  D6'6 -> '6'

c'D6z'D6 :: D6 -> D6z
c'D6z'D6 x = case x of
  D6'1 -> D6z'1
  D6'2 -> D6z'2
  D6'3 -> D6z'3
  D6'4 -> D6z'4
  D6'5 -> D6z'5
  D6'6 -> D6z'6

c'D6'D6z :: D6z -> Maybe D6
c'D6'D6z x = case x of
  D6z'0 -> Nothing
  D6z'1 -> Just D6'1
  D6z'2 -> Just D6'2
  D6z'3 -> Just D6'3
  D6z'4 -> Just D6'4
  D6z'5 -> Just D6'5
  D6z'6 -> Just D6'6

data D7z = D7z'0 | D7z'1 | D7z'2 | D7z'3 | D7z'4 | D7z'5 | D7z'6 | D7z'7
  deriving (Eq, Ord, Show)

instance Digit D7z where
  digitToInt x = case x of
    D7z'0 -> 0
    D7z'1 -> 1
    D7z'2 -> 2
    D7z'3 -> 3
    D7z'4 -> 4
    D7z'5 -> 5
    D7z'6 -> 6
    D7z'7 -> 7

  intToDigit x = case x of
    0 -> Just D7z'0
    1 -> Just D7z'1
    2 -> Just D7z'2
    3 -> Just D7z'3
    4 -> Just D7z'4
    5 -> Just D7z'5
    6 -> Just D7z'6
    7 -> Just D7z'7
    _ -> Nothing

instance Display D7z where display = (:) . c'Char'D7z

c'Char'D7z :: D7z -> Char
c'Char'D7z x = case x of
  D7z'0 -> '0'
  D7z'1 -> '1'
  D7z'2 -> '2'
  D7z'3 -> '3'
  D7z'4 -> '4'
  D7z'5 -> '5'
  D7z'6 -> '6'
  D7z'7 -> '7'

data D7 = D7'1 | D7'2 | D7'3 | D7'4 | D7'5 | D7'6 | D7'7
  deriving (Eq, Ord, Show)

instance Digit D7 where
  digitToInt x = case x of
    D7'1 -> 1
    D7'2 -> 2
    D7'3 -> 3
    D7'4 -> 4
    D7'5 -> 5
    D7'6 -> 6
    D7'7 -> 7

  intToDigit x = case x of
    1 -> Just D7'1
    2 -> Just D7'2
    3 -> Just D7'3
    4 -> Just D7'4
    5 -> Just D7'5
    6 -> Just D7'6
    7 -> Just D7'7
    _ -> Nothing

instance Display D7 where display = (:) . c'Char'D7

c'Char'D7 :: D7 -> Char
c'Char'D7 x = case x of
  D7'1 -> '1'
  D7'2 -> '2'
  D7'3 -> '3'
  D7'4 -> '4'
  D7'5 -> '5'
  D7'6 -> '6'
  D7'7 -> '7'

c'D7z'D7 :: D7 -> D7z
c'D7z'D7 x = case x of
  D7'1 -> D7z'1
  D7'2 -> D7z'2
  D7'3 -> D7z'3
  D7'4 -> D7z'4
  D7'5 -> D7z'5
  D7'6 -> D7z'6
  D7'7 -> D7z'7

c'D7'D7z :: D7z -> Maybe D7
c'D7'D7z x = case x of
  D7z'0 -> Nothing
  D7z'1 -> Just D7'1
  D7z'2 -> Just D7'2
  D7z'3 -> Just D7'3
  D7z'4 -> Just D7'4
  D7z'5 -> Just D7'5
  D7z'6 -> Just D7'6
  D7z'7 -> Just D7'7

data D8z = D8z'0 | D8z'1 | D8z'2 | D8z'3 | D8z'4 | D8z'5 | D8z'6 | D8z'7 | D8z'8
  deriving (Eq, Ord, Show)

instance Digit D8z where
  digitToInt x = case x of
    D8z'0 -> 0
    D8z'1 -> 1
    D8z'2 -> 2
    D8z'3 -> 3
    D8z'4 -> 4
    D8z'5 -> 5
    D8z'6 -> 6
    D8z'7 -> 7
    D8z'8 -> 8

  intToDigit x = case x of
    0 -> Just D8z'0
    1 -> Just D8z'1
    2 -> Just D8z'2
    3 -> Just D8z'3
    4 -> Just D8z'4
    5 -> Just D8z'5
    6 -> Just D8z'6
    7 -> Just D8z'7
    8 -> Just D8z'8
    _ -> Nothing

instance Display D8z where display = (:) . c'Char'D8z

c'Char'D8z :: D8z -> Char
c'Char'D8z x = case x of
  D8z'0 -> '0'
  D8z'1 -> '1'
  D8z'2 -> '2'
  D8z'3 -> '3'
  D8z'4 -> '4'
  D8z'5 -> '5'
  D8z'6 -> '6'
  D8z'7 -> '7'
  D8z'8 -> '8'

data D8 = D8'1 | D8'2 | D8'3 | D8'4 | D8'5 | D8'6 | D8'7 | D8'8
  deriving (Eq, Ord, Show)

instance Digit D8 where
  digitToInt x = case x of
    D8'1 -> 1
    D8'2 -> 2
    D8'3 -> 3
    D8'4 -> 4
    D8'5 -> 5
    D8'6 -> 6
    D8'7 -> 7
    D8'8 -> 8

  intToDigit x = case x of
    1 -> Just D8'1
    2 -> Just D8'2
    3 -> Just D8'3
    4 -> Just D8'4
    5 -> Just D8'5
    6 -> Just D8'6
    7 -> Just D8'7
    8 -> Just D8'8
    _ -> Nothing

instance Display D8 where display = (:) . c'Char'D8

c'Char'D8 :: D8 -> Char
c'Char'D8 x = case x of
  D8'1 -> '1'
  D8'2 -> '2'
  D8'3 -> '3'
  D8'4 -> '4'
  D8'5 -> '5'
  D8'6 -> '6'
  D8'7 -> '7'
  D8'8 -> '8'

c'D8z'D8 :: D8 -> D8z
c'D8z'D8 x = case x of
  D8'1 -> D8z'1
  D8'2 -> D8z'2
  D8'3 -> D8z'3
  D8'4 -> D8z'4
  D8'5 -> D8z'5
  D8'6 -> D8z'6
  D8'7 -> D8z'7
  D8'8 -> D8z'8

c'D8'D8z :: D8z -> Maybe D8
c'D8'D8z x = case x of
  D8z'0 -> Nothing
  D8z'1 -> Just D8'1
  D8z'2 -> Just D8'2
  D8z'3 -> Just D8'3
  D8z'4 -> Just D8'4
  D8z'5 -> Just D8'5
  D8z'6 -> Just D8'6
  D8z'7 -> Just D8'7
  D8z'8 -> Just D8'8

data D9z = D9z'0 | D9z'1 | D9z'2 | D9z'3 | D9z'4 | D9z'5 | D9z'6 | D9z'7 | D9z'8 | D9z'9
  deriving (Eq, Ord, Show)

instance Digit D9z where
  digitToInt x = case x of
    D9z'0 -> 0
    D9z'1 -> 1
    D9z'2 -> 2
    D9z'3 -> 3
    D9z'4 -> 4
    D9z'5 -> 5
    D9z'6 -> 6
    D9z'7 -> 7
    D9z'8 -> 8
    D9z'9 -> 9

  intToDigit x = case x of
    0 -> Just D9z'0
    1 -> Just D9z'1
    2 -> Just D9z'2
    3 -> Just D9z'3
    4 -> Just D9z'4
    5 -> Just D9z'5
    6 -> Just D9z'6
    7 -> Just D9z'7
    8 -> Just D9z'8
    9 -> Just D9z'9
    _ -> Nothing

instance Display D9z where display = (:) . c'Char'D9z

c'Char'D9z :: D9z -> Char
c'Char'D9z x = case x of
  D9z'0 -> '0'
  D9z'1 -> '1'
  D9z'2 -> '2'
  D9z'3 -> '3'
  D9z'4 -> '4'
  D9z'5 -> '5'
  D9z'6 -> '6'
  D9z'7 -> '7'
  D9z'8 -> '8'
  D9z'9 -> '9'

data D9 = D9'1 | D9'2 | D9'3 | D9'4 | D9'5 | D9'6 | D9'7 | D9'8 | D9'9
  deriving (Eq, Ord, Show)

instance Digit D9 where
  digitToInt x = case x of
    D9'1 -> 1
    D9'2 -> 2
    D9'3 -> 3
    D9'4 -> 4
    D9'5 -> 5
    D9'6 -> 6
    D9'7 -> 7
    D9'8 -> 8
    D9'9 -> 9

  intToDigit x = case x of
    1 -> Just D9'1
    2 -> Just D9'2
    3 -> Just D9'3
    4 -> Just D9'4
    5 -> Just D9'5
    6 -> Just D9'6
    7 -> Just D9'7
    8 -> Just D9'8
    9 -> Just D9'9
    _ -> Nothing

instance Display D9 where display = (:) . c'Char'D9

c'Char'D9 :: D9 -> Char
c'Char'D9 x = case x of
  D9'1 -> '1'
  D9'2 -> '2'
  D9'3 -> '3'
  D9'4 -> '4'
  D9'5 -> '5'
  D9'6 -> '6'
  D9'7 -> '7'
  D9'8 -> '8'
  D9'9 -> '9'

c'D9z'D9 :: D9 -> D9z
c'D9z'D9 x = case x of
  D9'1 -> D9z'1
  D9'2 -> D9z'2
  D9'3 -> D9z'3
  D9'4 -> D9z'4
  D9'5 -> D9z'5
  D9'6 -> D9z'6
  D9'7 -> D9z'7
  D9'8 -> D9z'8
  D9'9 -> D9z'9

c'D9'D9z :: D9z -> Maybe D9
c'D9'D9z x = case x of
  D9z'0 -> Nothing
  D9z'1 -> Just D9'1
  D9z'2 -> Just D9'2
  D9z'3 -> Just D9'3
  D9z'4 -> Just D9'4
  D9z'5 -> Just D9'5
  D9z'6 -> Just D9'6
  D9z'7 -> Just D9'7
  D9z'8 -> Just D9'8
  D9z'9 -> Just D9'9

