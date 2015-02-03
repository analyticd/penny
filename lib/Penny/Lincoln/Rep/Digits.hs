module Penny.Lincoln.Rep.Digits where

class Digit a where
  digitToInt :: Integral b => a -> b
  intToDigit :: Integral b => b -> Maybe a

data D0z = D0z'0
  deriving (Eq, Ord, Show)

instance Digit D0z where
  digitToInt x = case x of
    D0z'0 -> 0

  intToDigit x = case x of
    0 -> Just D0z'0
    _ -> Nothing

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

data D1 = D1'1
  deriving (Eq, Ord, Show)

instance Digit D1 where
  digitToInt x = case x of
    D1'1 -> 1

  intToDigit x = case x of
    1 -> Just D1'1
    _ -> Nothing

c'D1z'D1 :: D1 -> D1z
c'D1z'D1 x = case x of
  D1'1 -> D1z'1

c'D1'D1z :: D1z -> Maybe D1
c'D1'D1z x = case x of
  D1z'0 -> Nothing
  D1z'1 -> Just D1'1

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

