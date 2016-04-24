{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Penny.Copper.Decopperize where

import qualified Control.Lens as Lens
import Data.Foldable (toList)
import Data.Monoid ((<>))
import qualified Data.Text as X
import Data.Time (ZonedTime)
import qualified Data.Time as Time
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Pinchot (Loc, NonEmpty)
import qualified Pinchot
import Prelude hiding (length)

import Penny.Arrangement
import qualified Penny.Commodity as Commodity
import Penny.Copper.Grouping
import Penny.Copper.Terminalizers
import Penny.Copper.Types
import Penny.Decimal
import Penny.NonNegative (NonNegative, length)
import qualified Penny.NonNegative as NN
import Penny.NonZero (NonZero, c'Integer'NonZero)
import qualified Penny.NonZero as NZ
import Penny.Polar
import Penny.Positive (Positive)
import qualified Penny.Positive as Pos
import Penny.Realm
import qualified Penny.Scalar as Scalar
import Penny.SeqUtil (mapMaybe)
import qualified Penny.Tree as Tree
import qualified Penny.Trio as Trio

novDecsToPositive :: D1'9 t a -> Seq (D0'9 t a) -> Positive
novDecsToPositive n = finish . go NN.zero NN.zero
  where
    go !places !tot sq = case Lens.unsnoc sq of
      Nothing -> (places, tot)
      Just (xs, x) -> go (NN.next places)
        (((cD0'9 x) `NN.mult` (NN.ten `NN.pow` places))
          `NN.add` tot) xs
    finish (places, tot) = case NN.c'Positive'NonNegative tot of
      Nothing -> res
      Just totPos -> totPos `Pos.add` res
      where
        res = cD1'9 n `Pos.mult` (Pos.ten `Pos.pow` places)

c'Int'Integer :: Integer -> Int
c'Int'Integer i
  | i < fromIntegral (minBound :: Int)
      = error $ "integer too small: " ++ show i
  | i > fromIntegral (maxBound :: Int)
      = error $ "integer too large: " ++ show i
  | otherwise = fromIntegral i

cD1'9 :: D1'9 t a -> Positive
cD1'9 x = case x of
  D1'9'One _ -> Pos.one
  D1'9'Two _ -> Pos.two
  D1'9'Three _ -> Pos.three
  D1'9'Four _ -> Pos.four
  D1'9'Five _ -> Pos.five
  D1'9'Six _ -> Pos.six
  D1'9'Seven _ -> Pos.seven
  D1'9'Eight _ -> Pos.eight
  D1'9'Nine _ -> Pos.nine

cD0'8 :: D0'8 t a -> NonNegative
cD0'8 x = case x of
  D0'8'Zero _ -> NN.zero
  D0'8'One _ -> NN.one
  D0'8'Two _ -> NN.two
  D0'8'Three _ -> NN.three
  D0'8'Four _ -> NN.four
  D0'8'Five _ -> NN.five
  D0'8'Six _ -> NN.six
  D0'8'Seven _ -> NN.seven
  D0'8'Eight _ -> NN.eight

cD0'1 :: D0'1 t a -> NonNegative
cD0'1 x = case x of
  D0'1'Zero _ -> NN.zero
  D0'1'One _ -> NN.one

cD0'2 :: D0'2 t a -> NonNegative
cD0'2 x = case x of
  D0'2'Zero _ -> NN.zero
  D0'2'One _ -> NN.one
  D0'2'Two _ -> NN.two

cD0'3 :: D0'3 t a -> NonNegative
cD0'3 x = case x of
  D0'3'Zero _ -> NN.zero
  D0'3'One _ -> NN.one
  D0'3'Two _ -> NN.two
  D0'3'Three _ -> NN.three

cD0'5 :: D0'5 t a -> NonNegative
cD0'5 x = case x of
  D0'5'Zero _ -> NN.zero
  D0'5'One _ -> NN.one
  D0'5'Two _ -> NN.two
  D0'5'Three _ -> NN.three
  D0'5'Four _ -> NN.four
  D0'5'Five _ -> NN.five

cD0'9 :: D0'9 t a -> NonNegative
cD0'9 x = case x of
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

cD0'1'Opt :: D0'1'Opt t a -> NonNegative
cD0'1'Opt (D0'1'Opt m) = case m of
  Nothing -> NN.zero
  Just d -> cD0'1 d

cN0'19 :: N0'19 t a -> NonNegative
cN0'19 (N0'19 d1 d0)
  = (cD0'1'Opt d1)
  `NN.add` (cD0'9 d0)

cN20'23 :: N20'23 t a -> Positive
cN20'23 (N20'23 _ d3) = case NN.c'Positive'NonNegative . cD0'3 $ d3 of
  Nothing -> twenty
  Just p -> twenty `Pos.add` p
  where
    twenty = Pos.ten `Pos.mult` Pos.two

cHours :: Hours t a -> NonNegative
cHours (Hours'N0'19 n) = cN0'19 n
cHours (Hours'N20'23 n) = NN.c'NonNegative'Positive . cN20'23 $ n

cN0'59 :: N0'59 t a -> NonNegative
cN0'59 (N0'59 d1 d0)
  = (cD0'5 d1 `NN.mult` NN.ten)
  `NN.add` (cD0'9 d0)

cMinutes :: Minutes t a -> NonNegative
cMinutes (Minutes n) = cN0'59 n

cSeconds :: Seconds t a -> NonNegative
cSeconds (Seconds n) = cN0'59 n

cColonSeconds :: ColonSeconds t a -> NonNegative
cColonSeconds (ColonSeconds _ s) = cSeconds s

cColonSeconds'Opt :: ColonSeconds'Opt t a -> NonNegative
cColonSeconds'Opt (ColonSeconds'Opt m) = case m of
  Nothing -> NN.zero
  Just c -> cColonSeconds c

cTime :: Time t a -> Time.TimeOfDay
cTime (Time h _ m cs) = Time.TimeOfDay h' m' s'
  where
    h' = conv . cHours $ h
    m' = conv . cMinutes $ m
    s' = fromIntegral . conv . cColonSeconds'Opt $ cs
    conv = c'Int'Integer . NN.c'Integer'NonNegative

cWhitesTime :: WhitesTime t a -> Time.TimeOfDay
cWhitesTime (WhitesTime _ t) = cTime t

cWhitesTime'Opt :: WhitesTime'Opt t a -> Time.TimeOfDay
cWhitesTime'Opt (WhitesTime'Opt m) = case m of
  Nothing -> Time.midnight
  Just t -> cWhitesTime t

cPluMin :: Num n => PluMin t a -> n -> n
cPluMin (PluMin'Plus _) = id
cPluMin (PluMin'Minus _) = negate

cPluMin'Opt :: Num n => PluMin'Opt t a -> n -> n
cPluMin'Opt (PluMin'Opt m) = case m of
  Nothing -> id
  Just a -> cPluMin a

cZoneHrsMins :: ZoneHrsMins t a -> Time.TimeZone
cZoneHrsMins (ZoneHrsMins pm d3 d2 d1 d0) = Time.TimeZone mins False ""
  where
    mins = cPluMin pm . c'Int'Integer . NN.c'Integer'NonNegative
      $ (cD0'2 d3 `raise` NN.three)
      `NN.add` (cD0'3 d2 `raise` NN.two)
      `NN.add` (cD0'9 d1 `raise` NN.one)
      `NN.add` (cD0'9 d0)
    raise b p = b `NN.mult` (NN.ten `NN.pow` p)

cZone :: Zone t a -> Time.TimeZone
cZone (Zone _ z) = cZoneHrsMins z

cWhitesZone :: WhitesZone t a -> Time.TimeZone
cWhitesZone (WhitesZone _ z) = cZone z

cWhitesZone'Opt :: WhitesZone'Opt t a -> Time.TimeZone
cWhitesZone'Opt (WhitesZone'Opt m) = case m of
  Nothing -> Time.utc
  Just z -> cWhitesZone z

-- # Strings
cUnquotedStringNonDigitChar
  :: UnquotedStringNonDigitChar t a
  -> t
cUnquotedStringNonDigitChar (UnquotedStringNonDigitChar (c, _)) = c

cUnquotedStringNonDigitChar'Plus
  :: UnquotedStringNonDigitChar'Plus t a
  -> NonEmpty t
cUnquotedStringNonDigitChar'Plus (UnquotedStringNonDigitChar'Plus ne)
  = fmap cUnquotedStringNonDigitChar ne

cUnquotedCommodity
  :: UnquotedCommodity Char a
  -> Commodity.Commodity
cUnquotedCommodity (UnquotedCommodity p)
  = X.pack
  . toList
  . Pinchot.flatten
  . cUnquotedStringNonDigitChar'Plus
  $ p

cNonEscapedChar
  :: NonEscapedChar t a
  -> t
cNonEscapedChar (NonEscapedChar (t, _)) = t

cEscPayload
  :: EscPayload t a
  -> Maybe Char
cEscPayload x = case x of
  EscPayload'Backslash _ -> Just '\\'
  EscPayload'Newline _ -> Just '\n'
  EscPayload'DoubleQuote _ -> Just '"'
  EscPayload'Gap _ -> Nothing

cEscSeq :: EscSeq t a -> Maybe Char
cEscSeq (EscSeq _ p) = cEscPayload p

cQuotedChar :: QuotedChar Char a -> Maybe Char
cQuotedChar x = case x of
  QuotedChar'NonEscapedChar c -> Just $ cNonEscapedChar c
  QuotedChar'EscSeq c -> cEscSeq c

cQuotedChar'Star :: QuotedChar'Star Char a -> Seq Char
cQuotedChar'Star (QuotedChar'Star sq)
  = mapMaybe cQuotedChar sq

cQuotedString :: QuotedString Char a -> Seq Char
cQuotedString (QuotedString _ q _) = cQuotedChar'Star q

cQuotedCommodity :: QuotedCommodity Char a -> Commodity.Commodity
cQuotedCommodity (QuotedCommodity q)
  = X.pack
  . toList
  . cQuotedString
  $ q

cCommodity :: Commodity Char a -> Commodity.Commodity
cCommodity x = case x of
  Commodity'UnquotedCommodity c -> cUnquotedCommodity c
  Commodity'QuotedCommodity c -> cQuotedCommodity c

-- # Dates

cYear :: Year t a -> Int
cYear (Year d3 d2 d1 d0)
  = conv d3 * 1000 + conv d2 * 100
    + conv d1 * 10 + conv d0
  where
    conv = c'Int'Integer . NN.c'Integer'NonNegative . cD0'9

cMod4 :: Mod4 t a -> Int
cMod4 x = case x of
  { L04 _ _ -> 4; L08 _ _ -> 8; L12 _ _ -> 12; L16 _ _ -> 16; L20 _ _ -> 20;
    L24 _ _ -> 24; L28 _ _ -> 28; L32 _ _ -> 32; L36 _ _ -> 36;
    L40 _ _ -> 40; L44 _ _ -> 44; L48 _ _ -> 48; L52 _ _ -> 52;
    L56 _ _ -> 56; L60 _ _ -> 60; L64 _ _ -> 64; L68 _ _ -> 68;
    L72 _ _ -> 72; L76 _ _ -> 76; L80 _ _ -> 80; L84 _ _ -> 84;
    L88 _ _ -> 88; L92 _ _ -> 92; L96 _ _ -> 96 }

cCenturyLeapYear :: CenturyLeapYear t a -> Int
cCenturyLeapYear (LeapYear0 _ _ _ _) = 0
cCenturyLeapYear (LeapYearMod4 m4 _ _) = cMod4 m4 * 100

cNonCenturyLeapYear :: NonCenturyLeapYear t a -> Int
cNonCenturyLeapYear (NonCenturyLeapYear d2 d1 m4)
  = conv d2 * 1000 + conv d1 * 100 + cMod4 m4
  where
    conv = c'Int'Integer . NN.c'Integer'NonNegative . cD0'9

cLeapYear :: LeapYear t a -> Int
cLeapYear (LeapYear'CenturyLeapYear x) = cCenturyLeapYear x
cLeapYear (LeapYear'NonCenturyLeapYear x) = cNonCenturyLeapYear x

cDays28 :: Days28 t a -> Positive
cDays28 x = case x of
  D28'1to9 _ d -> cD1'9 d
  D28'10to19 _ d -> case NN.c'Positive'NonNegative (cD0'9 d) of
    Nothing -> Pos.ten
    Just p -> Pos.ten `Pos.add` p
  D28'20to28 _ d -> case NN.c'Positive'NonNegative (cD0'8 d) of
    Nothing -> Pos.ten `Pos.mult` Pos.two
    Just p -> (Pos.ten `Pos.mult` Pos.two) `Pos.add` p

cDays30 :: Days30 t a -> Positive
cDays30 (D30'28 d28) = cDays28 d28
cDays30 (D30'29 _ _) = (Pos.ten `Pos.mult` Pos.two) `Pos.add` Pos.nine
cDays30 (D30'30 _ _) = (Pos.ten `Pos.mult` Pos.three)

cDays31 :: Days31 t a -> Positive
cDays31 (D31'30 d30) = cDays30 d30
cDays31 (D31'31 _ _) = (Pos.ten `Pos.mult` Pos.three) `Pos.add` Pos.one

cMonthDay
  :: MonthDay t a
  -> (Positive, Positive)
  -- ^ Positive for the month (1 is January, 12 is December) and for
  -- day
cMonthDay x = case x of
  Jan _ _ _ d31 -> (Pos.one, cDays31 d31)
  Feb _ _ _ d28 -> (Pos.two, cDays28 d28)
  Mar _ _ _ d31 -> (Pos.three, cDays31 d31)
  Apr _ _ _ d30 -> (Pos.four, cDays30 d30)
  May _ _ _ d31 -> (Pos.five, cDays31 d31)
  Jun _ _ _ d30 -> (Pos.six, cDays30 d30)
  Jul _ _ _ d31 -> (Pos.seven, cDays31 d31)
  Aug _ _ _ d31 -> (Pos.eight, cDays31 d31)
  Sep _ _ _ d30 -> (Pos.nine, cDays30 d30)
  Oct _ _ _ d31 -> (Pos.ten, cDays31 d31)
  Nov _ _ _ d30 -> (Pos.ten `Pos.add` Pos.one, cDays30 d30)
  Dec _ _ _ d31 -> (Pos.ten `Pos.add` Pos.two, cDays31 d31)

cDate :: Date t a -> Time.Day
cDate x = case x of
  Date'NonLeapDay (NonLeapDay yr _ md) ->
    Time.fromGregorian (fromIntegral y) (conv m) (conv d)
    where
      y = cYear yr
      (m, d) = cMonthDay md
      conv = c'Int'Integer . Pos.c'Integer'Positive
  Date'LeapDay (LeapDay yr _ _ _ _ _ _) ->
    Time.fromGregorian (fromIntegral y) m d
    where
      y = cLeapYear yr
      m = 2
      d = 29

-- # Numbers

cNilUngroupedRadCom :: NilUngroupedRadCom t a -> DecZero
cNilUngroupedRadCom = Exponential () . e'NilUngroupedRadCom
  where
    e'NilUngroupedRadCom :: NilUngroupedRadCom t a -> NonNegative
    e'NilUngroupedRadCom (NUZeroRadCom _ (RadixZeroesRadCom'Opt Nothing))
      = NN.zero
    e'NilUngroupedRadCom (NUZeroRadCom _ (RadixZeroesRadCom'Opt
      (Just (RadixZeroesRadCom _ (Zero'Star sq))))) = length sq
    e'NilUngroupedRadCom (NURadixRadCom _ _z1 (Zero'Star zs))
      = NN.one `NN.add` (length zs)

cNilUngroupedRadPer :: NilUngroupedRadPer t a -> DecZero
cNilUngroupedRadPer = Exponential () . e'NilUngroupedRadPer
  where
    e'NilUngroupedRadPer :: NilUngroupedRadPer t a -> NonNegative
    e'NilUngroupedRadPer (NUZeroRadPer _ (RadixZeroesRadPer'Opt Nothing))
      = NN.zero
    e'NilUngroupedRadPer (NUZeroRadPer _ (RadixZeroesRadPer'Opt
      (Just (RadixZeroesRadPer _ (Zero'Star sq))))) = length sq
    e'NilUngroupedRadPer (NURadixRadPer _ _z1 (Zero'Star zs))
      = NN.one `NN.add` (length zs)

cNilGroupedRadCom :: NilGroupedRadCom t a -> DecZero
cNilGroupedRadCom = Exponential () . e'NilGroupedRadCom
  where
    e'NilGroupedRadCom :: NilGroupedRadCom t a -> NonNegative
    e'NilGroupedRadCom (NilGroupedRadCom _zMay _rdx _z1 zs1 zss)
      = NN.one `NN.add` zeroes1 `NN.add` zeroesRest
      where
        zeroes1 = let Zero'Star zs = zs1 in length zs
        zeroesRest = addGroup g1 (foldr addGroup NN.zero gs)
          where
            ZeroGroupRadCom'Plus (Pinchot.NonEmpty g1 gs) = zss
            addGroup (ZeroGroupRadCom _ _zero1 (Zero'Star zeros)) acc
              = NN.one `NN.add` length zeros `NN.add` acc

cNilGroupedRadPer :: NilGroupedRadPer t a -> DecZero
cNilGroupedRadPer = Exponential () . e'NilGroupedRadPer
  where
    e'NilGroupedRadPer :: NilGroupedRadPer t a -> NonNegative
    e'NilGroupedRadPer (NilGroupedRadPer _zMay _rdx _z1 zs1 zss)
      = NN.one `NN.add` zeroes1 `NN.add` zeroesRest
      where
        zeroes1 = let Zero'Star zs = zs1 in length zs
        zeroesRest = addGroup g1 (foldr addGroup NN.zero gs)
          where
            ZeroGroupRadPer'Plus (Pinchot.NonEmpty g1 gs) = zss
            addGroup (ZeroGroupRadPer _ _zero1 (Zero'Star zeros)) acc
              = NN.one `NN.add` length zeros `NN.add` acc

cNilRadCom :: NilRadCom t a -> DecZero
cNilRadCom x = case x of
  NilRadCom'NilUngroupedRadCom c -> cNilUngroupedRadCom c
  NilRadCom'NilGroupedRadCom c -> cNilGroupedRadCom c

cNilRadPer :: NilRadPer t a -> DecZero
cNilRadPer x = case x of
  NilRadPer'NilUngroupedRadPer c -> cNilUngroupedRadPer c
  NilRadPer'NilGroupedRadPer c -> cNilGroupedRadPer c

cNeutral :: Neutral t a -> DecZero
cNeutral (NeuCom _ n) = cNilRadCom n
cNeutral (NeuPer n) = cNilRadPer n

cRadixComDigits :: RadixComDigits t a -> NonNegative
cRadixComDigits (RadixComDigits _ (D0'9'Star sq)) = length sq

cRadixPerDigits :: RadixPerDigits t a -> NonNegative
cRadixPerDigits (RadixPerDigits _ (D0'9'Star sq)) = length sq

cRadixComDigits'Opt :: RadixComDigits'Opt t a -> NonNegative
cRadixComDigits'Opt (RadixComDigits'Opt may)
  = maybe NN.zero cRadixComDigits may

cRadixPerDigits'Opt :: RadixPerDigits'Opt t a -> NonNegative
cRadixPerDigits'Opt (RadixPerDigits'Opt may)
  = maybe NN.zero cRadixPerDigits may

cBrimUngroupedRadCom :: BrimUngroupedRadCom t a -> DecPositive
cBrimUngroupedRadCom (BUGreaterThanOneRadCom nv (D0'9'Star ds1)
  (RadixComDigits'Opt Nothing))
  = Exponential (novDecsToPositive nv ds1) NN.zero

cBrimUngroupedRadCom (BUGreaterThanOneRadCom nv (D0'9'Star ds1)
  (RadixComDigits'Opt (Just (RadixComDigits _ (D0'9'Star ds2)))))
  = Exponential (novDecsToPositive nv (ds1 <> ds2))
                (length ds2)

cBrimUngroupedRadCom
  (BULessThanOneRadCom _ _ (Zero'Star zs1) nv (D0'9'Star ds))
  = Exponential (novDecsToPositive nv ds)
                (NN.one `NN.add` (length zs1) `NN.add` (length ds))

cBrimUngroupedRadPer :: BrimUngroupedRadPer t a -> DecPositive
cBrimUngroupedRadPer (BUGreaterThanOneRadPer nv (D0'9'Star ds1)
  (RadixPerDigits'Opt Nothing))
  = Exponential (novDecsToPositive nv ds1) NN.zero

cBrimUngroupedRadPer (BUGreaterThanOneRadPer nv (D0'9'Star ds1)
  (RadixPerDigits'Opt (Just (RadixPerDigits _ (D0'9'Star ds2)))))
  = Exponential (novDecsToPositive nv (ds1 <> ds2))
                (length ds2)

cBrimUngroupedRadPer
  (BULessThanOneRadPer _ _ (Zero'Star zs1) nv (D0'9'Star ds))
  = Exponential (novDecsToPositive nv ds)
                (NN.one `NN.add` (length zs1) `NN.add` (length ds))

cBrimGroupedRadCom :: BrimGroupedRadCom t a -> DecPositive
cBrimGroupedRadCom
  = cBrimUngroupedRadCom . ungroupBrimGroupedRadCom

cBrimGroupedRadPer :: BrimGroupedRadPer t a -> DecPositive
cBrimGroupedRadPer
  = cBrimUngroupedRadPer . ungroupBrimGroupedRadPer

cBrimRadCom :: BrimRadCom t a -> DecPositive
cBrimRadCom
  = cBrimUngroupedRadCom . ungroupBrimRadCom

cBrimRadPer :: BrimRadPer t a -> DecPositive
cBrimRadPer
  = cBrimUngroupedRadPer . ungroupBrimRadPer

cNonNeutral :: NonNeutral t a -> DecPositive
cNonNeutral (NonNeutralRadCom _ b) = cBrimRadCom b
cNonNeutral (NonNeutralRadPer b) = cBrimRadPer b

cPluMinNonNeutral :: PluMinNonNeutral t a -> DecNonZero
cPluMinNonNeutral (PluMinNonNeutral pm _ nn)
  = c'DecNonZero'DecPositive pole (cNonNeutral nn)
  where
    pole = case pm of
      PluMin'Plus _ -> positive
      PluMin'Minus _ -> negative

cExchNonNeu :: ExchNonNeu t a -> DecNonZero
cExchNonNeu x = case x of
  ExchNonNeu'PluMinNonNeutral c -> cPluMinNonNeutral c
  ExchNonNeu'NonNeutral c -> c'DecNonZero'DecPositive positive
    . cNonNeutral $ c

cExch :: Exch t a -> Decimal
cExch x = case x of
  Exch'ExchNonNeu n -> fmap c'Integer'NonZero . cExchNonNeu $ n
  Exch'Neutral n -> fmap (const 0) . cNeutral $ n

cCyExch :: CyExch Char a -> (Commodity.Commodity, Decimal)
cCyExch (CyExch c _ e) = (cCommodity c, cExch e)

cExchCy :: ExchCy Char a -> (Commodity.Commodity, Decimal)
cExchCy (ExchCy e _ c) = (cCommodity c, cExch e)

cJanus :: Janus Char a -> (Commodity.Commodity, Decimal)
cJanus x = case x of
  Janus'CyExch c -> cCyExch c
  Janus'ExchCy c -> cExchCy c

data PriceParts = PriceParts
  { _pricePos :: Loc
  , _priceTime :: ZonedTime
  , _priceFrom :: Commodity.Commodity
  , _priceTo :: Commodity.Commodity
  , _priceExch :: Decimal
  }

type TxnParts = (Seq Tree.Tree, Seq (Loc, Trio.Trio, Seq Tree.Tree))

cPrice :: Price Char Loc -> PriceParts
cPrice p = PriceParts loc zt from to exch
  where
    loc = Lens.view (Lens.to _r'Price'0'AtSign . Lens._Wrapped' . Lens._2) p
    zt = Time.ZonedTime lt tz
    tod = cWhitesTime'Opt . _r'Price'3'WhitesTime'Opt $ p
    day = cDate . _r'Price'2'Date $ p
    tz = cWhitesZone'Opt . _r'Price'4'WhitesZone'Opt $ p
    lt = Time.LocalTime day tod
    from = cCommodity . _r'Price'6'Commodity $ p
    (to, exch) = cJanus . _r'Price'8'Janus $ p

cUnquotedStringNonFirstChar
  :: UnquotedStringNonFirstChar t a
  -> t
cUnquotedStringNonFirstChar x = case x of
  UnquotedStringNonFirstChar'UnquotedStringNonDigitChar c ->
    cUnquotedStringNonDigitChar c
  UnquotedStringNonFirstChar'D0'9 c -> case c of
    D0'9'Zero (Zero (a, _)) -> a
    D0'9'One (One (a, _)) -> a
    D0'9'Two (Two (a, _)) -> a
    D0'9'Three (Three (a, _)) -> a
    D0'9'Four (Four (a, _)) -> a
    D0'9'Five (Five (a, _)) -> a
    D0'9'Six (Six (a, _)) -> a
    D0'9'Seven (Seven (a, _)) -> a
    D0'9'Eight (Eight (a, _)) -> a
    D0'9'Nine (Nine (a, _)) -> a

cUnquotedStringNonFirstChar'Star
  :: UnquotedStringNonFirstChar'Star t a
  -> Seq t
cUnquotedStringNonFirstChar'Star (UnquotedStringNonFirstChar'Star sq)
  = fmap cUnquotedStringNonFirstChar sq

cUnquotedString
  :: UnquotedString t a
  -> Seq t
cUnquotedString (UnquotedString ds c1 cs)
  = digits ds <> (cUnquotedStringNonDigitChar c1 `Lens.cons`
      cUnquotedStringNonFirstChar'Star cs)
  where
    digits (D0'9'Star digs) = fmap f digs
      where
        f x = case x of
          D0'9'Zero (Zero (a, _)) -> a
          D0'9'One (One (a, _)) -> a
          D0'9'Two (Two (a, _)) -> a
          D0'9'Three (Three (a, _)) -> a
          D0'9'Four (Four (a, _)) -> a
          D0'9'Five (Five (a, _)) -> a
          D0'9'Six (Six (a, _)) -> a
          D0'9'Seven (Seven (a, _)) -> a
          D0'9'Eight (Eight (a, _)) -> a
          D0'9'Nine (Nine (a, _)) -> a

cWholeNonZero :: WholeNonZero t a -> NonZero
cWholeNonZero (WholeNonZero (PluMin'Opt mayPm) d1 (D0'9'Star ds))
  = flip . NZ.c'NonZero'Positive $ novDecsToPositive d1 ds
  where
    flip = case mayPm of
      Nothing -> id
      Just pm -> case pm of
        PluMin'Plus _ -> id
        PluMin'Minus _ -> NZ.negate

cWholeAny :: WholeAny t a -> Integer
cWholeAny x = case x of
  WholeAny'Zero _ -> 0
  WholeAny'WholeNonZero w -> NZ.c'Integer'NonZero . cWholeNonZero $ w

cScalar :: Scalar Char a -> Scalar.Scalar
cScalar x = case x of
  Scalar'UnquotedString a -> Scalar.SText . X.pack . toList
    . cUnquotedString $ a
  Scalar'QuotedString a -> Scalar.SText . X.pack . toList
    . cQuotedString $ a
  Scalar'Date a -> Scalar.SDay . cDate $ a
  Scalar'Time a -> Scalar.STime . cTime $ a
  Scalar'Zone a -> Scalar.SZone . Time.timeZoneMinutes . cZone $ a
  Scalar'WholeAny a -> Scalar.SInteger . cWholeAny $ a

cWhitesScalar :: WhitesScalar Char a -> Scalar.Scalar
cWhitesScalar (WhitesScalar _ s) = cScalar s

cWhitesScalar'Opt :: WhitesScalar'Opt Char a -> Maybe Scalar.Scalar
cWhitesScalar'Opt (WhitesScalar'Opt m) = fmap cWhitesScalar m

locToTree :: Loc -> Tree.Tree
locToTree l = Tree.Tree System scalarLoc
  [ intTree "line" (Pinchot._line l)
  , intTree "column" (Pinchot._col l)
  , intTree "position" (Pinchot._pos l)
  ]
  where
    scalarLoc = Just $ Scalar.SText "location"
    intTree txt int = Tree.Tree System scalarTxt [ treeInt ]
      where
        scalarTxt = Just $ Scalar.SText txt
        treeInt = Tree.Tree System
          (Just $ Scalar.SInteger (fromIntegral int)) []

cNextTree :: NextTree Char Loc -> Tree.Tree
cNextTree (NextTree _ _ _ t) = cTree t

cTree :: Tree Char Loc -> Tree.Tree
cTree x = Tree.Tree User sc (loc `Lens.cons` children)
  where
    loc = locToTree . snd . Pinchot._front . t'Tree $ x
    (sc, children) = case x of
      Tree'ScalarMaybeForest s -> (Just scalar, trees)
        where
          (scalar, trees) = cScalarMaybeForest s
      Tree'ForestMaybeScalar s -> (mayScalar, Pinchot.flatten trees)
        where
          (trees, mayScalar) = cForestMaybeScalar s

cForestMaybeScalar
  :: ForestMaybeScalar Char Loc
  -> (NonEmpty Tree.Tree, Maybe Scalar.Scalar)
cForestMaybeScalar (ForestMaybeScalar bf sc)
  = (cBracketedForest bf, cWhitesScalar'Opt sc)

cBracketedForest
  :: BracketedForest Char Loc
  -> NonEmpty Tree.Tree
cBracketedForest (BracketedForest _ _ forest _ _)
  = cForest forest

cForest
  :: Forest Char Loc
  -> NonEmpty Tree.Tree
cForest (Forest t1 ts)
  = Pinchot.NonEmpty (cTree t1) (cNextTree'Star ts)

cNextTree'Star
  :: NextTree'Star Char Loc
  -> Seq Tree.Tree
cNextTree'Star (NextTree'Star sq)
  = fmap cNextTree sq

cScalarMaybeForest
  :: ScalarMaybeForest Char Loc
  -> (Scalar.Scalar, Seq Tree.Tree)
cScalarMaybeForest (ScalarMaybeForest sc wbf)
  = (cScalar sc, cWhitesBracketedForest'Opt wbf)

cWhitesBracketedForest'Opt
  :: WhitesBracketedForest'Opt Char Loc
  -> Seq Tree.Tree
cWhitesBracketedForest'Opt (WhitesBracketedForest'Opt may) = case may of
  Nothing -> Seq.empty
  Just wbf -> Pinchot.flatten . cWhitesBracketedForest $ wbf

cWhitesBracketedForest
  :: WhitesBracketedForest Char Loc
  -> NonEmpty Tree.Tree
cWhitesBracketedForest (WhitesBracketedForest _ bf)
  = cBracketedForest bf

cTopLine :: TopLine Char Loc -> NonEmpty Tree.Tree
cTopLine (TopLine f) = cForest f

cDebitCredit :: DebitCredit t a -> Pole
cDebitCredit x = case x of
  DebitCredit'Debit _ -> debit
  DebitCredit'Credit _ -> credit

cT_DebitCredit :: T_DebitCredit t a -> Trio.Trio
cT_DebitCredit (T_DebitCredit x) = Trio.S $ cDebitCredit x

cT_DebitCredit_Commodity :: T_DebitCredit_Commodity Char a -> Trio.Trio
cT_DebitCredit_Commodity (T_DebitCredit_Commodity dc _ cy)
  = Trio.SC (cDebitCredit dc) (cCommodity cy)

cT_DebitCredit_NonNeutral
  :: T_DebitCredit_NonNeutral Char a -> Trio.Trio
cT_DebitCredit_NonNeutral dcnn = Trio.Q repAnyRadix
  where
    T_DebitCredit_NonNeutral dc _ nn2 = fmap (const ()) dcnn
    p = cDebitCredit dc
    repAnyRadix = case nn2 of
      NonNeutralRadCom _ brimRadCom ->
        Left $ Extreme  (Polarized brimRadCom p)
      NonNeutralRadPer brimRadPer ->
        Right $ Extreme (Polarized brimRadPer p)

cT_DebitCredit_Commodity_NonNeutral
  :: T_DebitCredit_Commodity_NonNeutral Char a
  -> Trio.Trio
cT_DebitCredit_Commodity_NonNeutral dcnn = Trio.QC repAnyRadix cy arrangement
  where
    T_DebitCredit_Commodity_NonNeutral dc0 _ c2 w3 nn4 = fmap (const ()) dcnn
    cy = cCommodity c2
    p = cDebitCredit dc0
    isSpace = not . Seq.null . t'White'Star $ w3
    repAnyRadix = case nn4 of
        NonNeutralRadCom _ brimRadCom ->
          Left $ Extreme  (Polarized brimRadCom p)
        NonNeutralRadPer brimRadPer ->
          Right $ Extreme (Polarized brimRadPer p)
    arrangement = Arrangement CommodityOnLeft isSpace

cT_DebitCredit_NonNeutral_Commodity
  :: T_DebitCredit_NonNeutral_Commodity Char a
  -> Trio.Trio
cT_DebitCredit_NonNeutral_Commodity dcnn = Trio.QC repAnyRadix cy arrangement
  where
    T_DebitCredit_NonNeutral_Commodity dc0 _ nn2 w3 c4 = fmap (const ()) dcnn
    cy = cCommodity c4
    p = cDebitCredit dc0
    isSpace = not . Seq.null . t'White'Star $ w3
    repAnyRadix = case nn2 of
        NonNeutralRadCom _ brimRadCom ->
          Left $ Extreme  (Polarized brimRadCom p)
        NonNeutralRadPer brimRadPer ->
          Right $ Extreme (Polarized brimRadPer p)
    arrangement = Arrangement CommodityOnRight isSpace

cT_Commodity :: T_Commodity Char a -> Trio.Trio
cT_Commodity (T_Commodity c) = Trio.C (cCommodity c)

cT_Commodity_Neutral :: T_Commodity_Neutral Char a -> Trio.Trio
cT_Commodity_Neutral cn
  = Trio.NC nilAnyRadix cy (Arrangement CommodityOnLeft isSpace)
  where
    T_Commodity_Neutral cy0 w1 n2 = fmap (const ()) cn
    cy = cCommodity cy0
    isSpace = not . Seq.null . t'White'Star $ w1
    nilAnyRadix = case n2 of
      NeuCom _ nilRadCom -> Left nilRadCom
      NeuPer nilRadPer -> Right nilRadPer

cT_Neutral_Commodity :: T_Neutral_Commodity Char a -> Trio.Trio
cT_Neutral_Commodity cn
  = Trio.NC nilAnyRadix cy (Arrangement CommodityOnRight isSpace)
  where
    T_Neutral_Commodity n0 w1 cy2 = fmap (const ()) cn
    cy = cCommodity cy2
    isSpace = not . Seq.null . t'White'Star $ w1
    nilAnyRadix = case n0 of
      NeuCom _ nilRadCom -> Left nilRadCom
      NeuPer nilRadPer -> Right nilRadPer

cT_Commodity_NonNeutral :: T_Commodity_NonNeutral Char a -> Trio.Trio
cT_Commodity_NonNeutral cnn
  = Trio.UC brimScalarAnyRadix cy (Arrangement CommodityOnLeft isSpace)
  where
    T_Commodity_NonNeutral cy0 w1 n2 = fmap (const ()) cnn
    cy = cCommodity cy0
    isSpace = not . Seq.null . t'White'Star $ w1
    brimScalarAnyRadix = case n2 of
      NonNeutralRadCom _ nilRadCom -> Left nilRadCom
      NonNeutralRadPer nilRadPer -> Right nilRadPer

cT_NonNeutral_Commodity :: T_NonNeutral_Commodity Char a -> Trio.Trio
cT_NonNeutral_Commodity cnn
  = Trio.UC brimScalarAnyRadix cy (Arrangement CommodityOnRight isSpace)
  where
    T_NonNeutral_Commodity n0 w1 cy2 = fmap (const ()) cnn
    cy = cCommodity cy2
    isSpace = not . Seq.null . t'White'Star $ w1
    brimScalarAnyRadix = case n0 of
      NonNeutralRadCom _ nilRadCom -> Left nilRadCom
      NonNeutralRadPer nilRadPer -> Right nilRadPer

cT_Neutral :: T_Neutral Char a -> Trio.Trio
cT_Neutral (T_Neutral n0) = Trio.UU nilAnyRadix
  where
    nilAnyRadix = case fmap (const ()) n0 of
      NeuCom _ nilRadCom -> Left nilRadCom
      NeuPer nilRadPer -> Right nilRadPer

cT_NonNeutral :: T_NonNeutral Char a -> Trio.Trio
cT_NonNeutral (T_NonNeutral n0) = Trio.US brimScalarAnyRadix
  where
    brimScalarAnyRadix = case fmap (const ()) n0 of
        NonNeutralRadCom _ brimRadCom -> Left brimRadCom
        NonNeutralRadPer brimRadPer -> Right brimRadPer

cTrio :: Trio Char a -> Trio.Trio
cTrio x = case x of
  Trio'T_DebitCredit a -> cT_DebitCredit a
  Trio'T_DebitCredit_Commodity a -> cT_DebitCredit_Commodity a
  Trio'T_DebitCredit_NonNeutral a -> cT_DebitCredit_NonNeutral a
  Trio'T_DebitCredit_Commodity_NonNeutral a ->
    cT_DebitCredit_Commodity_NonNeutral a
  Trio'T_DebitCredit_NonNeutral_Commodity a ->
    cT_DebitCredit_NonNeutral_Commodity a
  Trio'T_Commodity a -> cT_Commodity a
  Trio'T_Commodity_Neutral a -> cT_Commodity_Neutral a
  Trio'T_Neutral_Commodity a -> cT_Neutral_Commodity a
  Trio'T_Commodity_NonNeutral a -> cT_Commodity_NonNeutral a
  Trio'T_NonNeutral_Commodity a -> cT_NonNeutral_Commodity a
  Trio'T_Neutral a -> cT_Neutral a
  Trio'T_NonNeutral a -> cT_NonNeutral a

cTrioMaybeForest
  :: TrioMaybeForest Char Loc
  -> (Trio.Trio, Seq Tree.Tree)
cTrioMaybeForest (TrioMaybeForest t wbf)
  = (cTrio t, cWhitesBracketedForest'Opt wbf)

cPosting
  :: Posting Char Loc
  -> (Loc, Trio.Trio, Seq Tree.Tree)
cPosting x = case x of
  Posting'TrioMaybeForest tmf -> (\(a, b) -> (loc, a, b))
    $ cTrioMaybeForest tmf
  Posting'BracketedForest bf -> (loc, Trio.E, Pinchot.flatten ts)
    where
      ts = cBracketedForest bf
  where
    loc = snd . Pinchot._front . t'Posting $ x

cNextPosting
  :: NextPosting Char Loc
  -> (Loc, Trio.Trio, Seq Tree.Tree)
cNextPosting (NextPosting _ _ _ p) = cPosting p

cNextPosting'Star
  :: NextPosting'Star Char Loc
  -> Seq (Loc, Trio.Trio, Seq Tree.Tree)
cNextPosting'Star (NextPosting'Star x) = fmap cNextPosting x

cPostingList
  :: PostingList Char Loc
  -> NonEmpty (Loc, Trio.Trio, Seq Tree.Tree)
cPostingList (PostingList _ p1 ps)
  = Pinchot.NonEmpty (cPosting p1) (cNextPosting'Star ps)

cPostingList'Opt
  :: PostingList'Opt Char Loc
  -> Seq (Loc, Trio.Trio, Seq Tree.Tree)
cPostingList'Opt (PostingList'Opt m) = case m of
  Nothing -> Seq.empty
  Just l -> Pinchot.flatten . cPostingList $ l

cPostings
  :: Postings Char Loc
  -> Seq (Loc, Trio.Trio, Seq Tree.Tree)
cPostings (Postings _ pl _ _) = cPostingList'Opt pl

cWhitesPostings
  :: WhitesPostings Char Loc
  -> Seq (Loc, Trio.Trio, Seq Tree.Tree)
cWhitesPostings (WhitesPostings _ p) = cPostings p

cWhitesPostings'Opt
  :: WhitesPostings'Opt Char Loc
  -> Seq (Loc, Trio.Trio, Seq Tree.Tree)
cWhitesPostings'Opt (WhitesPostings'Opt m)
  = maybe Seq.empty cWhitesPostings m

cTopLineMaybePostings
  :: TopLineMaybePostings Char Loc
  -> (NonEmpty Tree.Tree, Seq (Loc, Trio.Trio, Seq Tree.Tree))
cTopLineMaybePostings (TopLineMaybePostings t w)
  = (cTopLine t, cWhitesPostings'Opt w)

cTransaction
  :: Transaction Char Loc
  -> (Seq Tree.Tree, Seq (Loc, Trio.Trio, Seq Tree.Tree))
cTransaction x = case x of
  Transaction'TopLineMaybePostings t ->
    Lens.over Lens._1 Pinchot.flatten $ cTopLineMaybePostings t
  Transaction'Postings p ->
    (\pairs -> (Seq.empty, pairs)) (cPostings p)

cFileItem
  :: FileItem Char Loc
  -> Either PriceParts TxnParts
cFileItem x = case x of
  FileItem'Price p -> Left $ cPrice p
  FileItem'Transaction t -> Right $ cTransaction t

cWhitesFileItem
  :: WhitesFileItem Char Loc
  -> Either PriceParts TxnParts
cWhitesFileItem (WhitesFileItem _ i) = cFileItem i

cWhitesFileItem'Star
  :: WhitesFileItem'Star Char Loc
  -> Seq (Either PriceParts TxnParts)
cWhitesFileItem'Star (WhitesFileItem'Star sq)
  = fmap cWhitesFileItem sq

cWholeFile
  :: WholeFile Char Loc
  -> Seq (Either PriceParts TxnParts)
cWholeFile (WholeFile x _) = cWhitesFileItem'Star x

cNilOrBrimRadCom
  :: NilOrBrimRadCom t a
  -> DecUnsigned
cNilOrBrimRadCom x = case x of
  NilOrBrimRadCom'NilRadCom nrc -> fmap (const NN.zero) (cNilRadCom nrc)
  NilOrBrimRadCom'BrimRadCom brc -> fmap NN.c'NonNegative'Positive
    (cBrimRadCom brc)

cNilOrBrimRadPer
  :: NilOrBrimRadPer t a
  -> DecUnsigned
cNilOrBrimRadPer x = case x of
  NilOrBrimRadPer'NilRadPer nrc -> fmap (const NN.zero) (cNilRadPer nrc)
  NilOrBrimRadPer'BrimRadPer brc -> fmap NN.c'NonNegative'Positive
    (cBrimRadPer brc)
