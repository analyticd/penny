{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Representing digits.
module Penny.Digit where

import Penny.Copper.Types

import Control.Applicative ((<|>))

{-

-- * Typeclass for any digit

class Digit a where
  digitToInt :: Integral b => a -> b
  intToDigit :: Integral b => b -> Maybe a

instance Digit Zero where
  digitToInt (Zero _) = 0
  intToDigit 0 = Just (Zero '0')
  intToDigit _ = Nothing

instance Digit One where
  digitToInt (One _) = 1
  intToDigit 1 = Just (One '1')
  intToDigit _ = Nothing

instance Digit Two where
  digitToInt (Two _) = 2
  intToDigit 2 = Just (Two '2')
  intToDigit _ = Nothing

instance Digit Three where
  digitToInt (Three _) = 3
  intToDigit 3 = Just (Three '3')
  intToDigit _ = Nothing

instance Digit Four where
  digitToInt (Four _) = 4
  intToDigit 4 = Just (Four '4')
  intToDigit _ = Nothing

instance Digit Five where
  digitToInt (Five _) = 5
  intToDigit 5 = Just (Five '5')
  intToDigit _ = Nothing

instance Digit Six where
  digitToInt (Six _) = 6
  intToDigit 6 = Just (Six '6')
  intToDigit _ = Nothing

instance Digit Seven where
  digitToInt (Seven _) = 7
  intToDigit 7 = Just (Seven '7')
  intToDigit _ = Nothing

instance Digit Eight where
  digitToInt (Eight _) = 8
  intToDigit 9 = Just (Eight '8')
  intToDigit _ = Nothing

instance Digit Nine where
  digitToInt (Nine _) = 9
  intToDigit 9 = Just (Nine '9')
  intToDigit _ = Nothing

instance Digit D0'9 where
  digitToInt x = case x of
    D0'9'Zero _ -> 0
    D0'9'One _ -> 1
    D0'9'Two _ -> 2
    D0'9'Three _ -> 3
    D0'9'Four _ -> 4
    D0'9'Five _ -> 5
    D0'9'Six _ -> 6
    D0'9'Seven _ -> 7
    D0'9'Eight _ -> 8
    D0'9'Nine _ -> 9

  intToDigit x = case x of
    0 -> Just $ D0'9'Zero (Zero '0')
    1 -> Just $ D0'9'One (One '1')
    2 -> Just $ D0'9'Two (Two '2')
    3 -> Just $ D0'9'Three (Three '3')
    4 -> Just $ D0'9'Four (Four '4')
    5 -> Just $ D0'9'Five (Five '5')
    6 -> Just $ D0'9'Six (Six '6')
    7 -> Just $ D0'9'Seven (Seven '7')
    8 -> Just $ D0'9'Eight (Eight '8')
    9 -> Just $ D0'9'Nine (Nine '9')
    _ -> Nothing

instance Digit D1'9 where
  digitToInt x = case x of
    D1'9'One _ -> 1
    D1'9'Two _ -> 2
    D1'9'Three _ -> 3
    D1'9'Four _ -> 4
    D1'9'Five _ -> 5
    D1'9'Six _ -> 6
    D1'9'Seven _ -> 7
    D1'9'Eight _ -> 8
    D1'9'Nine _ -> 9

  intToDigit x = case x of
    1 -> Just $ D1'9'One (One '1')
    2 -> Just $ D1'9'Two (Two '2')
    3 -> Just $ D1'9'Three (Three '3')
    4 -> Just $ D1'9'Four (Four '4')
    5 -> Just $ D1'9'Five (Five '5')
    6 -> Just $ D1'9'Six (Six '6')
    7 -> Just $ D1'9'Seven (Seven '7')
    8 -> Just $ D1'9'Eight (Eight '8')
    9 -> Just $ D1'9'Nine (Nine '9')
    _ -> Nothing

instance Digit D0'8 where
  digitToInt x = case x of
    D0'8'Zero _ -> 0
    D0'8'One _ -> 1
    D0'8'Two _ -> 2
    D0'8'Three _ -> 3
    D0'8'Four _ -> 4
    D0'8'Five _ -> 5
    D0'8'Six _ -> 6
    D0'8'Seven _ -> 7
    D0'8'Eight _ -> 8

  intToDigit x = case x of
    0 -> Just $ D0'8'Zero (Zero '0')
    1 -> Just $ D0'8'One (One '1')
    2 -> Just $ D0'8'Two (Two '2')
    3 -> Just $ D0'8'Three (Three '3')
    4 -> Just $ D0'8'Four (Four '4')
    5 -> Just $ D0'8'Five (Five '5')
    6 -> Just $ D0'8'Six (Six '6')
    7 -> Just $ D0'8'Seven (Seven '7')
    8 -> Just $ D0'8'Eight (Eight '8')
    _ -> Nothing

instance Digit D0'1 where
  digitToInt x = case x of
    D0'1'Zero _ -> 0
    D0'1'One _ -> 1

  intToDigit x = case x of
    0 -> Just $ D0'1'Zero (Zero '0')
    1 -> Just $ D0'1'One (One '1')
    _ -> Nothing


instance Digit D0'2 where
  digitToInt x = case x of
    D0'2'Zero _ -> 0
    D0'2'One _ -> 1
    D0'2'Two _ -> 2

  intToDigit x = case x of
    0 -> Just $ D0'2'Zero (Zero '0')
    1 -> Just $ D0'2'One (One '1')
    2 -> Just $ D0'2'Two (Two '2')
    _ -> Nothing

instance Digit D0'3 where
  digitToInt x = case x of
    D0'3'Zero _ -> 0
    D0'3'One _ -> 1
    D0'3'Two _ -> 2
    D0'3'Three _ -> 3

  intToDigit x = case x of
    0 -> Just $ D0'3'Zero (Zero '0')
    1 -> Just $ D0'3'One (One '1')
    2 -> Just $ D0'3'Two (Two '2')
    3 -> Just $ D0'3'Three (Three '3')
    _ -> Nothing

instance Digit D0'5 where
  digitToInt x = case x of
    D0'5'Zero _ -> 0
    D0'5'One _ -> 1
    D0'5'Two _ -> 2
    D0'5'Three _ -> 3
    D0'5'Four _ -> 4
    D0'5'Five _ -> 5

  intToDigit x = case x of
    0 -> Just $ D0'5'Zero (Zero '0')
    1 -> Just $ D0'5'One (One '1')
    2 -> Just $ D0'5'Two (Two '2')
    3 -> Just $ D0'5'Three (Three '3')
    4 -> Just $ D0'5'Four (Four '4')
    5 -> Just $ D0'5'Five (Five '5')
    _ -> Nothing

-- # Dates

instance Digit Days28 where
  digitToInt (D28'1to9 _ d) = digitToInt d
  digitToInt (D28'10to19 _ d) = 10 + digitToInt d
  digitToInt (D28'20to28 _ d) = 20 + digitToInt d

  intToDigit x = d1to9 <|> d10to19 <|> d20to28
    where
      d1to9 = do
        d <- intToDigit x
        return $ D28'1to9 zero d
      d10to19 = do
        d <- intToDigit (x - 10)
        return $ D28'10to19 one d
      d20to28 = do
        d <- intToDigit (x - 20)
        return $ D28'20to28 two d

instance Digit Days30 where
  digitToInt (D30'28 d28) = digitToInt d28
  digitToInt (D30'29 _ _) = 29
  digitToInt (D30'30 _ _) = 30

  intToDigit x = d28 <|> d29 <|> d30
    where
      d28 = fmap D30'28 (intToDigit x)
      d29 | x == 29 = Just $ D30'29 two nine
          | otherwise = Nothing
      d30 | x == 30 = Just $ D30'30 three zero
          | otherwise = Nothing

instance Digit Days31 where
  digitToInt (D31'30 d30) = digitToInt d30
  digitToInt (D31'31 _ _) = 31

  intToDigit x = d30 <|> d31
    where
      d30 = intToDigit x
      d31 | x == 31 = Just $ D31'31 three one
          | otherwise = Nothing

instance Digit Year where
  digitToInt (Year d3 d2 d1 d0)
    = digitToInt d3 * 1000 + digitToInt d2 * 100
      + digitToInt d1 * 10 + digitToInt d0
  intToDigit x = do
    let (r0, id0) = x `divMod` 10
    d0 <- intToDigit id0
    let (r1, id1) = r0 `divMod` 10
    d1 <- intToDigit id1
    let (r2, id2) = r1 `divMod` 10
    d2 <- intToDigit id2
    let (_, id3) = r2 `divMod` 10
    d3 <- intToDigit id3
    return $ Year d3 d2 d1 d0

instance Digit Mod4 where
  digitToInt x = case x of
    { L04 _ _ -> 4; L08 _ _ -> 8; L12 _ _ -> 12; L16 _ _ -> 16; L20 _ _ -> 20;
      L24 _ _ -> 24; L28 _ _ -> 28; L32 _ _ -> 32; L36 _ _ -> 36;
      L40 _ _ -> 40; L44 _ _ -> 44; L48 _ _ -> 48; L52 _ _ -> 52;
      L56 _ _ -> 56; L60 _ _ -> 60; L64 _ _ -> 64; L68 _ _ -> 68;
      L72 _ _ -> 72; L76 _ _ -> 76; L80 _ _ -> 80; L84 _ _ -> 84;
      L88 _ _ -> 88; L92 _ _ -> 92; L96 _ _ -> 96 }

  intToDigit x = case x of
    { 4 -> Just $ L04 zero four; 8 -> Just $ L08 zero eight;
      12 -> Just $ L12 one two;
      16 -> Just $ L16 one six; 20 -> Just $ L20 two zero;
      24 -> Just $ L24 two four;
      28 -> Just $ L28 two eight; 32 -> Just $ L32 three two;
      36 -> Just $ L36 three six;
      40 -> Just $ L40 four zero; 44 -> Just $ L44 four four;
      48 -> Just $ L48 four eight;
      52 -> Just $ L52 five two; 56 -> Just $ L56 five six;
      60 -> Just $ L60 six zero;
      64 -> Just $ L64 six four; 68 -> Just $ L68 six eight;
      72 -> Just $ L72 seven two;
      76 -> Just $ L76 seven six; 80 -> Just $ L80 eight zero;
      84 -> Just $ L84 eight four;
      88 -> Just $ L88 eight eight; 92 -> Just $ L92 nine two;
      96 -> Just $ L96 nine six;
      _ -> Nothing }

instance Digit CenturyLeapYear where
  digitToInt (LeapYear0 _ _ _ _) = 0
  digitToInt (LeapYearMod4 m4 _ _) = digitToInt m4 * 100

  intToDigit x
    | x == 0 = Just $ LeapYear0 zero zero zero zero
    | rm == 0 = do
        m4 <- intToDigit dv
        return $ LeapYearMod4 m4 zero zero
    | otherwise = Nothing
    where
      (dv, rm) = x `divMod` 100

instance Digit NonCenturyLeapYear where
  digitToInt (NonCenturyLeapYear d2 d1 m4)
    = digitToInt d2 * 1000 + digitToInt d1 * 100 + digitToInt m4

  intToDigit x
    | rm == 0 = Nothing
    | otherwise = do
        m4 <- intToDigit rm
        let (r1, id1) = dv `divMod` 10
        d1 <- intToDigit id1
        d2 <- intToDigit r1
        return $ NonCenturyLeapYear d2 d1 m4
    where
      (dv, rm) = x `divMod` 100

instance Digit LeapYear where
  digitToInt (LeapYear'CenturyLeapYear x) = digitToInt x
  digitToInt (LeapYear'NonCenturyLeapYear x) = digitToInt x

  intToDigit x = fmap LeapYear'CenturyLeapYear (intToDigit x)
    <|> fmap LeapYear'NonCenturyLeapYear (intToDigit x)


instance Digit N0'19 where
  digitToInt (N0'19 (D0'1'Maybe mayD1) d9) = case mayD1 of
    Nothing -> digitToInt d9
    Just d1 -> digitToInt d1 * 10 + digitToInt d9
  intToDigit x = d10'19 <|> d0'9
    where
      d10'19 = do
        d1 <- intToDigit (x - 10)
        return (N0'19 (D0'1'Maybe (Just (D0'1'One (One '1')))) d1)
      d0'9 = intToDigit x

instance Digit N20'23 where
  digitToInt (N20'23 _ d2) = 20 + digitToInt d2
  intToDigit x = do
    d1 <- intToDigit $ x - 20
    return $ N20'23 (Two '2') d1

instance Digit Hours where
  digitToInt (Hours'N0'19 x) = digitToInt x
  digitToInt (Hours'N20'23 x) = digitToInt x
  intToDigit i = Hours'N0'19 <$> intToDigit i
    <|> Hours'N20'23 <$> intToDigit i

instance Digit N0'59 where
  digitToInt (N0'59 d5 d9) = digitToInt d5 * 10 + digitToInt d9
  intToDigit x = do
    let (r0, intDigit0) = x `divMod` 10
    d0 <- intToDigit intDigit0
    d1 <- intToDigit r0
    return $ N0'59 d1 d0

deriving instance Digit Minutes
deriving instance Digit Seconds


class HasZero a where
  zero :: a

instance HasZero Zero where
  zero = Zero '0'

instance HasZero D0'9 where
  zero = D0'9'Zero zero

class HasOne a where
  one :: a

instance HasOne D0'9 where
  one = D0'9'One one

instance HasOne One where
  one = One '1'

class HasTwo a where
  two :: a

instance HasTwo D0'9 where
  two = D0'9'Two two

instance HasTwo Two where
  two = Two '2'

class HasThree a where
  three :: a

instance HasThree D0'9 where
  three = D0'9'Three three

instance HasThree Three where
  three = Three '3'

class HasFour a where
  four :: a

instance HasFour D0'9 where
  four = D0'9'Four four

instance HasFour Four where
  four = Four '4'

class HasFive a where
  five :: a

instance HasFive Five where
  five = Five '5'

instance HasFive D0'9 where
  five = D0'9'Five five

class HasSix a where
  six :: a

instance HasSix Six where
  six = Six '6'

instance HasSix D0'9 where
  six = D0'9'Six six

class HasSeven a where
  seven :: a

instance HasSeven Seven where
  seven = Seven '7'

instance HasSeven D0'9 where
  seven = D0'9'Seven seven

class HasEight a where
  eight :: a

instance HasEight Eight where
  eight = Eight '8'

instance HasEight D0'9 where
  eight = D0'9'Eight eight

class HasNine a where
  nine :: a

instance HasNine D0'9 where
  nine = D0'9'Nine nine

instance HasNine Nine where
  nine = Nine '9'

class HasTen a where
  ten :: a
-}
