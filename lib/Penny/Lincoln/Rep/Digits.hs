module Penny.Lincoln.Rep.Digits where

data Novem
  = D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Eq, Ord, Show)

data Decem
  = D0
  | Nonem Novem
  deriving (Eq, Ord, Show)

class OneToNine a where
  fromNovem :: Novem -> a

class OneToNine a => HasZero a where
  fromDecem :: Decem -> a

class Digit a where
  digitToInt :: Integral b => a -> b
  intToDigit :: Integral b => b -> Maybe a

instance Digit Novem where
  digitToInt n = case n of
    { D1 -> 1; D2 -> 2; D3 -> 3; D4 -> 4; D5 -> 5; D6 -> 6;
      D7 -> 7; D8 -> 8; D9 -> 9 }
  intToDigit n = case n of
    { 1 -> Just D1; 2 -> Just D2; 3 -> Just D3; 4 -> Just D4;
      5 -> Just D5; 6 -> Just D6; 7 -> Just D7; 8 -> Just D8;
      9 -> Just D9; _ -> Nothing }

instance Digit Decem where
  digitToInt n = case n of
    D0 -> 0
    Nonem x -> digitToInt x
  intToDigit n = case n of
    0 -> Just D0
    x -> fmap Nonem $ intToDigit x


