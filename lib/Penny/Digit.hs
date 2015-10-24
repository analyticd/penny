module Penny.Digit where

import Penny.Grammar

class Digit a where
  digitToInt :: Integral b => a -> b
  intToDigit :: Integral b => b -> Maybe a

instance Digit Zero where
  digitToInt x = case x of
    Zero -> 0

  intToDigit x = case x of
    0 -> Just Zero
    _ -> Nothing

instance Digit One where
  digitToInt x = case x of
    One -> 1

  intToDigit x = case x of
    1 -> Just One
    _ -> Nothing

instance Digit Two where
  digitToInt x = case x of
    Two -> 2

  intToDigit x = case x of
    2 -> Just Two
    _ -> Nothing

instance Digit Three where
  digitToInt x = case x of
    Three -> 3

  intToDigit x = case x of
    3 -> Just Three
    _ -> Nothing

instance Digit Four where
  digitToInt x = case x of
    Four -> 4

  intToDigit x = case x of
    4 -> Just Four
    _ -> Nothing

instance Digit Five where
  digitToInt x = case x of
    Five -> 5

  intToDigit x = case x of
    5 -> Just Five
    _ -> Nothing

instance Digit Six where
  digitToInt x = case x of
    Six -> 6

  intToDigit x = case x of
    6 -> Just Six
    _ -> Nothing

instance Digit Seven where
  digitToInt x = case x of
    Seven -> 7

  intToDigit x = case x of
    7 -> Just Seven
    _ -> Nothing

instance Digit Eight where
  digitToInt x = case x of
    Eight -> 8

  intToDigit x = case x of
    8 -> Just Eight
    _ -> Nothing

instance Digit Nine where
  digitToInt x = case x of
    Nine -> 9

  intToDigit x = case x of
    9 -> Just Nine
    _ -> Nothing

instance Digit D1z where
  digitToInt x = case x of
    D1z'0 -> 0
    D1z'1 -> 1

  intToDigit x = case x of
    0 -> Just D1z'0
    1 -> Just D1z'1
    _ -> Nothing

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

instance Digit D2 where
  digitToInt x = case x of
    D2'1 -> 1
    D2'2 -> 2

  intToDigit x = case x of
    1 -> Just D2'1
    2 -> Just D2'2
    _ -> Nothing

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

