{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Conversions between Copper types and other types.

module Penny.Copper.Conversions where

import Penny.Copper.Types
import Penny.NonNegative (NonNegative)
import qualified Penny.NonNegative as NN
import Penny.Positive (Positive)
import qualified Penny.Positive as Pos

import Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

one :: One
one = One '1'

two :: Two
two = Two '2'

three :: Three
three = Three '3'

four :: Four
four = Four '4'

five :: Five
five = Five '5'

six :: Six
six = Six '6'

seven :: Seven
seven = Seven '7'

eight :: Eight
eight = Eight '8'

nine :: Nine
nine = Nine '9'

zero :: Zero
zero = Zero '0'

c'NonNegative'D0'9 :: D0'9 -> NonNegative
c'NonNegative'D0'9 x = case x of
  D0'9'Zero _ -> NN.zero
  D0'9'One _ -> NN.one
  D0'9'Two _ -> NN.two
  D0'9'Three _ -> NN.three
  D0'9'Four _ -> NN.four
  D0'9'Five _ -> NN.five
  D0'9'Six _ -> NN.six
  D0'9'Seven _ -> NN.seven
  D0'9'Eight _ -> NN.eight
  D0'9'Nine _ -> NN.nine

c'Int'D0'9 :: D0'9 -> Int
c'Int'D0'9 x = case x of
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

c'D0'9'Int :: Integral a => a -> Maybe D0'9
c'D0'9'Int x = case x of
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

c'Positive'D1'9 :: D1'9 -> Positive
c'Positive'D1'9 x = case x of
  D1'9'One _ -> Pos.one
  D1'9'Two _ -> Pos.two
  D1'9'Three _ -> Pos.three
  D1'9'Four _ -> Pos.four
  D1'9'Five _ -> Pos.five
  D1'9'Six _ -> Pos.six
  D1'9'Seven _ -> Pos.seven
  D1'9'Eight _ -> Pos.eight
  D1'9'Nine _ -> Pos.nine

novDecsToPositive :: D1'9 -> Seq D0'9 -> Positive
novDecsToPositive n = finish . go NN.zero NN.zero
  where
    go !places !tot sq = case Lens.unsnoc sq of
      Nothing -> (places, tot)
      Just (xs, x) -> go (NN.next places)
        (((c'NonNegative'D0'9 x) `NN.mult` (NN.ten `NN.pow` places))
          `NN.add` tot) xs
    finish (places, tot) = case NN.c'Positive'NonNegative tot of
      Nothing -> res
      Just totPos -> totPos `Pos.add` res
      where
        res = c'Positive'D1'9 n `Pos.mult` (Pos.ten `Pos.pow` places)

c'Int'D1'9 :: D1'9 -> Int
c'Int'D1'9 x = case x of
  D1'9'One _ -> 1
  D1'9'Two _ -> 2
  D1'9'Three _ -> 3
  D1'9'Four _ -> 4
  D1'9'Five _ -> 5
  D1'9'Six _ -> 6
  D1'9'Seven _ -> 7
  D1'9'Eight _ -> 8
  D1'9'Nine _ -> 9

c'D1'9'Int :: Integral a => a -> Maybe D1'9
c'D1'9'Int x = case x of
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

c'Int'D0'8 :: D0'8 -> Int
c'Int'D0'8 x = case x of
  D0'8'Zero _ -> 0
  D0'8'One _ -> 1
  D0'8'Two _ -> 2
  D0'8'Three _ -> 3
  D0'8'Four _ -> 4
  D0'8'Five _ -> 5
  D0'8'Six _ -> 6
  D0'8'Seven _ -> 7
  D0'8'Eight _ -> 8

c'D0'8'Int :: Integral a => a -> Maybe D0'8
c'D0'8'Int x = case x of
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

c'Int'D0'1 :: D0'1 -> Int
c'Int'D0'1 x = case x of
  D0'1'Zero _ -> 0
  D0'1'One _ -> 1

c'D0'1'Int :: Integral a => a -> Maybe D0'1
c'D0'1'Int x = case x of
  0 -> Just $ D0'1'Zero (Zero '0')
  1 -> Just $ D0'1'One (One '1')
  _ -> Nothing

c'Int'D0'2 :: D0'2 -> Int
c'Int'D0'2 x = case x of
  D0'2'Zero _ -> 0
  D0'2'One _ -> 1
  D0'2'Two _ -> 2

c'D0'2'Int :: Integral a => a -> Maybe D0'2
c'D0'2'Int x = case x of
  0 -> Just $ D0'2'Zero (Zero '0')
  1 -> Just $ D0'2'One (One '1')
  2 -> Just $ D0'2'Two (Two '2')
  _ -> Nothing

c'Int'D0'3 :: D0'3 -> Int
c'Int'D0'3 x = case x of
  D0'3'Zero _ -> 0
  D0'3'One _ -> 1
  D0'3'Two _ -> 2
  D0'3'Three _ -> 3

c'D0'3'Int :: Integral a => a -> Maybe D0'3
c'D0'3'Int x = case x of
  0 -> Just $ D0'3'Zero (Zero '0')
  1 -> Just $ D0'3'One (One '1')
  2 -> Just $ D0'3'Two (Two '2')
  3 -> Just $ D0'3'Three (Three '3')
  _ -> Nothing

c'Int'D0'5 :: D0'5 -> Int
c'Int'D0'5 x = case x of
  D0'5'Zero _ -> 0
  D0'5'One _ -> 1
  D0'5'Two _ -> 2
  D0'5'Three _ -> 3
  D0'5'Four _ -> 4
  D0'5'Five _ -> 5

c'D0'5'Int :: Integral a => a -> Maybe D0'5
c'D0'5'Int x = case x of
  0 -> Just $ D0'5'Zero (Zero '0')
  1 -> Just $ D0'5'One (One '1')
  2 -> Just $ D0'5'Two (Two '2')
  3 -> Just $ D0'5'Three (Three '3')
  4 -> Just $ D0'5'Four (Four '4')
  5 -> Just $ D0'5'Five (Five '5')
  _ -> Nothing


-- # Dates and times

c'Int'Days28 :: Days28 -> Int
c'Int'Days28 x = case x of
  D28'1to9 _ d -> c'Int'D1'9 d
  D28'10to19 _ d -> 10 + c'Int'D0'9 d
  D28'20to28 _ d -> 20 + c'Int'D0'8 d

c'Days28'Int :: Integral a => a -> Maybe Days28
c'Days28'Int x = d1to9 <|> d10to19 <|> d20to28
  where
    d1to9 = do
      d <- c'D1'9'Int x
      return $ D28'1to9 (Zero '0') d
    d10to19 = do
      d <- c'D0'9'Int (x - 10)
      return $ D28'10to19 (One '1') d
    d20to28 = do
      d <- c'D0'8'Int (x - 20)
      return $ D28'20to28 (Two '2') d

c'Int'Days30 :: Days30 -> Int
c'Int'Days30 (D30'28 d28) = c'Int'Days28 d28
c'Int'Days30 (D30'29 _ _) = 29
c'Int'Days30 (D30'30 _ _) = 30

c'Days30'Int :: Integral a => a -> Maybe Days30
c'Days30'Int x = d28 <|> d29 <|> d30
  where
    d28 = fmap D30'28 (c'Days28'Int x)
    d29 | x == 29 = Just $ D30'29 (Two '2') (Nine '9')
        | otherwise = Nothing
    d30 | x == 30 = Just $ D30'30 (Three '3') (Zero '0')
        | otherwise = Nothing

c'Int'Days31 :: Days31 -> Int
c'Int'Days31 (D31'30 d30) = c'Int'Days30 d30
c'Int'Days31 (D31'31 _ _) = 31

c'Days31'Int :: Integral a => a -> Maybe Days31
c'Days31'Int x = d30 <|> d31
  where
    d30 = fmap D31'30 $ c'Days30'Int x
    d31 | x == 31 = Just $ D31'31 (Three '3') (One '1')
        | otherwise = Nothing

c'Int'Year :: Year -> Int
c'Int'Year (Year d3 d2 d1 d0)
  = c'Int'D0'9 d3 * 1000 + c'Int'D0'9 d2 * 100
    + c'Int'D0'9 d1 * 10 + c'Int'D0'9 d0

c'Year'Int :: Integral a => a -> Maybe Year
c'Year'Int x = do
  let (r0, id0) = x `divMod` 10
  d0 <- c'D0'9'Int id0
  let (r1, id1) = r0 `divMod` 10
  d1 <- c'D0'9'Int id1
  let (r2, id2) = r1 `divMod` 10
  d2 <- c'D0'9'Int id2
  let (_, id3) = r2 `divMod` 10
  d3 <- c'D0'9'Int id3
  return $ Year d3 d2 d1 d0

c'Int'Mod4 :: Mod4 -> Int
c'Int'Mod4 x = case x of
  { L04 _ _ -> 4; L08 _ _ -> 8; L12 _ _ -> 12; L16 _ _ -> 16; L20 _ _ -> 20;
    L24 _ _ -> 24; L28 _ _ -> 28; L32 _ _ -> 32; L36 _ _ -> 36;
    L40 _ _ -> 40; L44 _ _ -> 44; L48 _ _ -> 48; L52 _ _ -> 52;
    L56 _ _ -> 56; L60 _ _ -> 60; L64 _ _ -> 64; L68 _ _ -> 68;
    L72 _ _ -> 72; L76 _ _ -> 76; L80 _ _ -> 80; L84 _ _ -> 84;
    L88 _ _ -> 88; L92 _ _ -> 92; L96 _ _ -> 96 }

c'Mod4'Int :: Integral a => a -> Maybe Mod4
c'Mod4'Int x = case x of
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

c'Int'CenturyLeapYear :: CenturyLeapYear -> Int
c'Int'CenturyLeapYear (LeapYear0 _ _ _ _) = 0
c'Int'CenturyLeapYear (LeapYearMod4 m4 _ _) = c'Int'Mod4 m4 * 100

c'CenturyLeapYear'Int :: Integral a => a -> Maybe CenturyLeapYear
c'CenturyLeapYear'Int x
  | x == 0 = Just $ LeapYear0 zero zero zero zero
  | rm == 0 = do
      m4 <- c'Mod4'Int dv
      return $ LeapYearMod4 m4 zero zero
  | otherwise = Nothing
  where
    (dv, rm) = x `divMod` 100

c'Int'NonCenturyLeapYear :: NonCenturyLeapYear -> Int
c'Int'NonCenturyLeapYear (NonCenturyLeapYear d2 d1 m4)
    = c'Int'D0'9 d2 * 1000 + c'Int'D0'9 d1 * 100 + c'Int'Mod4 m4

c'NonCenturyLeapYear'Int :: Integral a => a -> Maybe NonCenturyLeapYear
c'NonCenturyLeapYear'Int x
  | rm == 0 = Nothing
  | otherwise = do
      m4 <- c'Mod4'Int rm
      let (r1, id1) = dv `divMod` 10
      d1 <- c'D0'9'Int id1
      d2 <- c'D0'9'Int r1
      return $ NonCenturyLeapYear d2 d1 m4
  where
    (dv, rm) = x `divMod` 100

c'Int'LeapYear :: LeapYear -> Int
c'Int'LeapYear (LeapYear'CenturyLeapYear x) = c'Int'CenturyLeapYear x
c'Int'LeapYear (LeapYear'NonCenturyLeapYear x) = c'Int'NonCenturyLeapYear x

c'LeapYear'Int :: Integral a => a -> Maybe LeapYear
c'LeapYear'Int x = fmap LeapYear'CenturyLeapYear (c'CenturyLeapYear'Int x)
    <|> fmap LeapYear'NonCenturyLeapYear (c'NonCenturyLeapYear'Int x)

c'Int'N0'19 :: N0'19 -> Int
c'Int'N0'19 (N0'19 (D0'1'Maybe mayD1) d9) = case mayD1 of
    Nothing -> c'Int'D0'9 d9
    Just d1 -> c'Int'D0'1 d1 * 10 + c'Int'D0'9 d9

c'N0'19'Int :: Integral a => a -> Maybe N0'19
c'N0'19'Int x = d10'19 <|> d0'9
  where
    d10'19 = do
      d1 <- c'D0'9'Int (x - 10)
      return (N0'19 (D0'1'Maybe (Just (D0'1'One one))) d1)
    d0'9 = do
      d0'9 <- c'D0'9'Int x
      return $ N0'19 (D0'1'Maybe Nothing) d0'9

c'Int'N20'23 :: N20'23 -> Int
c'Int'N20'23 (N20'23 _ d2) = 20 + c'Int'D0'3 d2

c'N20'23'Int :: Integral a => a -> Maybe N20'23
c'N20'23'Int x = do
  d1 <- c'D0'3'Int $ x - 20
  return $ N20'23 two d1

c'Int'Hours :: Hours -> Int
c'Int'Hours (Hours'N0'19 x) = c'Int'N0'19 x
c'Int'Hours (Hours'N20'23 x) = c'Int'N20'23 x

c'Hours'Int :: Integral a => a -> Maybe Hours
c'Hours'Int i = Hours'N0'19 <$> c'N0'19'Int i
    <|> Hours'N20'23 <$> c'N20'23'Int i

c'Int'N0'59 :: N0'59 -> Int
c'Int'N0'59 (N0'59 d5 d9) = c'Int'D0'5 d5 * 10 + c'Int'D0'9 d9

c'N0'59'Int :: Integral a => a -> Maybe N0'59
c'N0'59'Int x = do
  let (r0, intDigit0) = x `divMod` 10
  d0 <- c'D0'9'Int intDigit0
  d1 <- c'D0'5'Int r0
  return $ N0'59 d1 d0

c'Int'Minutes :: Minutes -> Int
c'Int'Minutes (Minutes x) = c'Int'N0'59 x

c'Minutes'Int :: Integral a => a -> Maybe Minutes
c'Minutes'Int = fmap Minutes . c'N0'59'Int

c'Int'Seconds :: Seconds -> Int
c'Int'Seconds (Seconds x) = c'Int'N0'59 x

c'Seconds'Int :: Integral a => a -> Maybe Seconds
c'Seconds'Int = fmap Seconds . c'N0'59'Int

-- | Transform a 'Positive' into its component digits.
positiveDigits
  :: Positive
  -> (D1'9, Seq D0'9)
positiveDigits pos = go (Pos.c'Integer'Positive pos) Seq.empty
  where
    go leftOver acc
      | quotient == 0 = (lastDigit, acc)
      | otherwise = go quotient (thisDigit `Lens.cons` acc)
      where
        (quotient, remainder) = leftOver `divMod` 10
        thisDigit = case c'D0'9'Int remainder of
          Just d -> d
          Nothing -> error "positiveDigits: error 1"
        lastDigit = case c'D1'9'Int remainder of
          Just d -> d
          Nothing -> error "positiveDigits: error 2"

