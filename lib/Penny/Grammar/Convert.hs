module Penny.Grammar.Convert where

import Penny.Digit
import Control.Applicative ((<|>))
import Penny.Grammar
import Data.Time

c'Char'D1z :: D1z -> Char
c'Char'D1z x = case x of
  D1z'0 -> '0'
  D1z'1 -> '1'

c'Char'D2z :: D2z -> Char
c'Char'D2z x = case x of
  D2z'0 -> '0'
  D2z'1 -> '1'
  D2z'2 -> '2'

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

c'Char'D3z :: D3z -> Char
c'Char'D3z x = case x of
  D3z'0 -> '0'
  D3z'1 -> '1'
  D3z'2 -> '2'
  D3z'3 -> '3'

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

c'Char'D4z :: D4z -> Char
c'Char'D4z x = case x of
  D4z'0 -> '0'
  D4z'1 -> '1'
  D4z'2 -> '2'
  D4z'3 -> '3'
  D4z'4 -> '4'

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

c'Char'D5z :: D5z -> Char
c'Char'D5z x = case x of
  D5z'0 -> '0'
  D5z'1 -> '1'
  D5z'2 -> '2'
  D5z'3 -> '3'
  D5z'4 -> '4'
  D5z'5 -> '5'

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

c'Char'D6z :: D6z -> Char
c'Char'D6z x = case x of
  D6z'0 -> '0'
  D6z'1 -> '1'
  D6z'2 -> '2'
  D6z'3 -> '3'
  D6z'4 -> '4'
  D6z'5 -> '5'
  D6z'6 -> '6'

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

decimalPlace :: (Digit a, Integral b) => Int -> a -> b
decimalPlace pl dig = digitToInt dig * 10 ^ pl

c'Int'DigitsFour :: Integral a => DigitsFour -> a
c'Int'DigitsFour (DigitsFour d3 d2 d1 d0)
  = decimalPlace 3 d3
  + decimalPlace 2 d2
  + decimalPlace 1 d1
  + decimalPlace 0 d0

c'Int'Digits1or2 :: Integral a => Digits1or2 -> a
c'Int'Digits1or2 (Digits1or2 l mayR) = case mayR of
  Nothing -> decimalPlace 0 l
  Just r -> decimalPlace 1 l + decimalPlace 0 r

c'Int'Hours :: Hours -> Int
c'Int'Hours h = case h of
  H0to19 mayD1 d9 -> d1 * 10 + digitToInt d9
    where d1 = maybe 0 digitToInt mayD1
  H20to23 _ d3 -> 20 + digitToInt d3

c'Int'ZeroTo59 :: ZeroTo59 -> Int
c'Int'ZeroTo59 (ZeroTo59 mayD5 d9)
  = ((maybe 0 digitToInt mayD5) * 10) + digitToInt d9


c'Days28'Int :: Integral a => a -> Maybe Days28
c'Days28'Int a
  | a > 0 && a < 10 = do
      d <- intToDigit a
      return $ D28'1to9 Zero d
  | a >= 10 && a < 20 = do
      d <- intToDigit (a - 10)
      return $ D28'10to19 One d
  | a >= 20 && a < 29 = do
      d <- intToDigit (a - 20)
      return $ D28'20to28 Two d
  | otherwise = Nothing

c'Int'Days28 :: Integral a => Days28 -> a
c'Int'Days28 day = case day of
  D28'1to9 _ d9 -> digitToInt d9
  D28'10to19 _ d9z -> digitToInt d9z + 10
  D28'20to28 _ d8 -> digitToInt d8 + 20

c'Days30'Int :: Integral a => a -> Maybe Days30
c'Days30'Int a
  | a == 29 = Just $ D30'29 Two Nine
  | a == 30 = Just $ D30'30 Three Zero
  | otherwise = fmap D30'28 $ c'Days28'Int a

c'Int'Days30 :: Integral a => Days30 -> a
c'Int'Days30 days = case days of
  D30'28 d28 -> c'Int'Days28 d28
  D30'29 _ _ -> 29
  D30'30 _ _ -> 30

c'Days31'Int :: Integral a => a -> Maybe Days31
c'Days31'Int a
  | a == 31 = Just $ D31'31 Three One
  | otherwise = fmap D31'30 $ c'Days30'Int a

c'Int'Days31 :: Integral a => Days31 -> a
c'Int'Days31 days31 = case days31 of
  D31'30 d30 -> c'Int'Days30 d30
  D31'31 _ _ -> 31

c'MonthDay'Ints
  :: (Integral a, Integral b)
  => DateSep
  -> a
  -- ^ Month
  -> b
  -- ^ Day
  -> Maybe MonthDay
c'MonthDay'Ints sep m d
  | m == 1 = (Jan Zero One sep) <$> c'Days31'Int d
  | m == 2 = (Feb Zero Two sep) <$> c'Days28'Int d
  | m == 3 = (Mar Zero Three sep) <$> c'Days31'Int d
  | m == 4 = (Apr Zero Four sep) <$> c'Days30'Int d
  | m == 5 = (May Zero Five sep) <$> c'Days31'Int d
  | m == 6 = (Jun Zero Six sep) <$> c'Days30'Int d
  | m == 7 = (Jul Zero Seven sep) <$> c'Days31'Int d
  | m == 8 = (Aug Zero Eight sep) <$> c'Days31'Int d
  | m == 9 = (Sep Zero Nine sep) <$> c'Days30'Int d
  | m == 10 = (Oct One Zero sep) <$> c'Days31'Int d
  | m == 11 = (Nov One One sep) <$> c'Days30'Int d
  | m == 12 = (Dec One Two sep) <$> c'Days31'Int d
  | otherwise = Nothing

intsFromMonthDay
  :: (Integral a, Integral b)
  => MonthDay
  -> (a, b)
intsFromMonthDay md = case md of
  Jan _ _ _ d -> (1, c'Int'Days31 d)
  Feb _ _ _ d -> (2, c'Int'Days28 d)
  Mar _ _ _ d -> (3, c'Int'Days31 d)
  Apr _ _ _ d -> (4, c'Int'Days30 d)
  May _ _ _ d -> (5, c'Int'Days31 d)
  Jun _ _ _ d -> (6, c'Int'Days30 d)
  Jul _ _ _ d -> (7, c'Int'Days31 d)
  Aug _ _ _ d -> (8, c'Int'Days31 d)
  Sep _ _ _ d -> (9, c'Int'Days30 d)
  Oct _ _ _ d -> (10, c'Int'Days31 d)
  Nov _ _ _ d -> (11, c'Int'Days30 d)
  Dec _ _ _ d -> (12, c'Int'Days31 d)

c'Int'Year :: Integral a => Year -> a
c'Int'Year (Year d3 d2 d1 d0)
  = places 3 d3 + places 2 d2 + places 1 d1 + places 0 d0
  where
    places p d = digitToInt d * 10 ^ (p :: Int)

c'Year'Int :: Integral a => a -> Maybe Year
c'Year'Int a
  | a < 0 = Nothing
  | a > 9999 = Nothing
  | otherwise = do
      let (thou, remThou) = a `divMod` 1000
      d0 <- intToDigit thou
      let (hun, remHun) = remThou `divMod` 100
      d1 <- intToDigit hun
      let (ten, remTen) = remHun `divMod` 10
      d2 <- intToDigit ten
      d3 <- intToDigit remTen
      return $ Year d0 d1 d2 d3

c'Int'Mod4 :: Integral a => Mod4 -> a
c'Int'Mod4 m4 = case m4 of
  { L00 _ _ -> 00; L04 _ _ -> 04; L08 _ _ -> 08; L12 _ _ -> 12;
    L16 _ _ -> 16;
    L20 _ _ -> 20; L24 _ _ -> 24; L28 _ _ -> 28; L32 _ _ -> 32;
    L36 _ _ -> 36; L40 _ _ -> 40; L44 _ _ -> 44; L48 _ _ -> 48;
    L52 _ _ -> 52; L56 _ _ -> 56; L60 _ _ -> 60; L64 _ _ -> 64;
    L68 _ _ -> 68; L72 _ _ -> 72; L76 _ _ -> 76; L80 _ _ -> 80;
    L84 _ _ -> 84; L88 _ _ -> 88; L92 _ _ -> 92; L96 _ _ -> 96 }

c'Mod4'Int :: Integral a => a -> Maybe Mod4
c'Mod4'Int a = case a of
  0 -> Just $ L00 Zero Zero
  4 -> Just $ L04 Zero Four
  8 -> Just $ L08 Zero Eight
  12 -> Just $ L12 One Two
  16 -> Just $ L16 One Six
  20 -> Just $ L20 Two Zero
  24 -> Just $ L24 Two Four
  28 -> Just $ L28 Two Eight
  32 -> Just $ L32 Three Two
  36 -> Just $ L36 Three Six
  40 -> Just $ L40 Four Zero
  44 -> Just $ L44 Four Four
  48 -> Just $ L48 Four Eight
  52 -> Just $ L52 Five Two
  56 -> Just $ L56 Five Six
  60 -> Just $ L60 Six Zero
  64 -> Just $ L64 Six Four
  68 -> Just $ L68 Six Eight
  72 -> Just $ L72 Seven Two
  76 -> Just $ L76 Seven Six
  80 -> Just $ L80 Eight Zero
  84 -> Just $ L84 Eight Four
  88 -> Just $ L88 Eight Eight
  92 -> Just $ L92 Nine Two
  96 -> Just $ L96 Nine Six
  _ -> Nothing


c'Int'CenturyLeapYear :: Integral a => CenturyLeapYear -> a
c'Int'CenturyLeapYear (CenturyLeapYear m4 _ _) =
  c'Int'Mod4 m4 * 100

c'Int'NonCenturyLeapYear :: Integral a => NonCenturyLeapYear -> a
c'Int'NonCenturyLeapYear (NonCenturyLeapYear d0 d1 m4)
  = places 3 d0 + places 2 d1 + c'Int'Mod4 m4
  where
    places p d = digitToInt d * 10 ^ (p :: Int)

c'Day'Date :: Date -> Day
c'Day'Date (Date ei) = maybe (error "c'Day'DateA: error") id
  $ fromGregorianValid yi mi di
  where
    (yi, mi, di) = case ei of
      Left (NonLeapDay y _ md) -> (c'Int'Year y, m, d)
        where
          (m, d) = intsFromMonthDay md
      Right (LeapDay eiYear _ _ _ _ _ _) ->
        ( either c'Int'CenturyLeapYear c'Int'NonCenturyLeapYear eiYear
        , 2, 29)

c'CenturyLeapYear'Int :: Integral a => a -> Maybe CenturyLeapYear
c'CenturyLeapYear'Int a
  | a < 0 = Nothing
  | a > 9999 = Nothing
  | rm /= 0 = Nothing
  | otherwise = do
      m4 <- c'Mod4'Int qt
      return $ CenturyLeapYear m4 Zero Zero
  where
    (qt, rm) = a `divMod` 100

c'NonCenturyLeapYear'Int :: Integral a => a -> Maybe NonCenturyLeapYear
c'NonCenturyLeapYear'Int a
  | a > 9999 = Nothing
  | a < 0 = Nothing
  | otherwise = do
      let (thou, rmThou) = a `divMod` 1000
      d0 <- intToDigit thou
      let (hun, rmHun) = rmThou `divMod` 100
      d1 <- intToDigit hun
      m4 <- c'Mod4'Int rmHun
      return $ NonCenturyLeapYear d0 d1 m4

c'Date'Day :: DateSep -> Day -> Maybe Date
c'Date'Day sep dy
  | m == 2 && d == 29 = fmap (Date . Right) leapDay
  | otherwise = fmap (Date . Left) nonLeapDay
  where
    (y, m, d) = toGregorian dy
    leapDay = do
      yr <-     fmap Left (c'CenturyLeapYear'Int y)
            <|> fmap Right (c'NonCenturyLeapYear'Int y)
      return $ LeapDay yr sep Zero Two sep Two Nine
    nonLeapDay = do
      yr <- c'Year'Int y
      md <- c'MonthDay'Ints sep m d
      return $ NonLeapDay yr sep md


c'NilOrBrimScalarAnyRadix'BrimScalarAnyRadix
  :: BrimScalarAnyRadix
  -> NilOrBrimScalarAnyRadix
c'NilOrBrimScalarAnyRadix'BrimScalarAnyRadix
  = either (Left . Right) (Right . Right)

