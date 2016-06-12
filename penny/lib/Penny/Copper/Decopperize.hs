{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Obtaining transactions and prices fro a Copper-formatted file
-- takes three steps: parsing, decopperization, and proofing.  This
-- module performs decopperization.
--
-- The top level function is 'dWholeFile'.
--
-- Decopperization never fails.
module Penny.Copper.Decopperize where

import qualified Control.Lens as Lens
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Monoid ((<>))
import qualified Data.Text as X
import qualified Data.Time as Time
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Pinchot (Loc, NonEmpty(_front))
import qualified Pinchot
import Prelude hiding (length)

import qualified Penny.Amount as A
import Penny.Arrangement
import Penny.Balance
import qualified Penny.Commodity as Commodity
import Penny.Copper.Grouping
import Penny.Copper.PriceParts
import Penny.Copper.Terminalizers
import Penny.Copper.Types
import Penny.Decimal
import Penny.NonNegative (NonNegative, length, zero, one)
import qualified Penny.NonNegative as NN
import Penny.NonZero (NonZero, c'Integer'NonZero)
import qualified Penny.NonZero as NZ
import Penny.Polar
import Penny.Positive (Positive)
import qualified Penny.Positive as Pos
import Penny.Rep
import qualified Penny.Scalar as Scalar
import Penny.SeqUtil (mapMaybe, catMaybes)
import qualified Penny.Tree as Tree
import qualified Penny.Trio as Trio
import qualified Penny.Troika as Troika

novDecsToPositive :: D1'9 t a -> Seq (D0'9 t a) -> Positive
novDecsToPositive n = finish . go NN.zero NN.zero
  where
    go !places !tot sq = case Lens.unsnoc sq of
      Nothing -> (places, tot)
      Just (xs, x) -> go (NN.next places)
        (((dD0'9 x) `NN.mult` (NN.ten `NN.pow` places))
          `NN.add` tot) xs
    finish (places, tot) = case NN.c'Positive'NonNegative tot of
      Nothing -> res
      Just totPos -> totPos `Pos.add` res
      where
        res = dD1'9 n `Pos.mult` (Pos.ten `Pos.pow` places)

c'Int'Integer :: Integer -> Int
c'Int'Integer i
  | i < fromIntegral (minBound :: Int)
      = error $ "integer too small: " ++ show i
  | i > fromIntegral (maxBound :: Int)
      = error $ "integer too large: " ++ show i
  | otherwise = fromIntegral i

dD1'9 :: D1'9 t a -> Positive
dD1'9 x = case x of
  D1'9'One _ -> Pos.one
  D1'9'Two _ -> Pos.two
  D1'9'Three _ -> Pos.three
  D1'9'Four _ -> Pos.four
  D1'9'Five _ -> Pos.five
  D1'9'Six _ -> Pos.six
  D1'9'Seven _ -> Pos.seven
  D1'9'Eight _ -> Pos.eight
  D1'9'Nine _ -> Pos.nine

dD0'8 :: D0'8 t a -> NonNegative
dD0'8 x = case x of
  D0'8'Zero _ -> NN.zero
  D0'8'One _ -> NN.one
  D0'8'Two _ -> NN.two
  D0'8'Three _ -> NN.three
  D0'8'Four _ -> NN.four
  D0'8'Five _ -> NN.five
  D0'8'Six _ -> NN.six
  D0'8'Seven _ -> NN.seven
  D0'8'Eight _ -> NN.eight

dD0'1 :: D0'1 t a -> NonNegative
dD0'1 x = case x of
  D0'1'Zero _ -> NN.zero
  D0'1'One _ -> NN.one

dD0'2 :: D0'2 t a -> NonNegative
dD0'2 x = case x of
  D0'2'Zero _ -> NN.zero
  D0'2'One _ -> NN.one
  D0'2'Two _ -> NN.two

dD0'3 :: D0'3 t a -> NonNegative
dD0'3 x = case x of
  D0'3'Zero _ -> NN.zero
  D0'3'One _ -> NN.one
  D0'3'Two _ -> NN.two
  D0'3'Three _ -> NN.three

dD0'5 :: D0'5 t a -> NonNegative
dD0'5 x = case x of
  D0'5'Zero _ -> NN.zero
  D0'5'One _ -> NN.one
  D0'5'Two _ -> NN.two
  D0'5'Three _ -> NN.three
  D0'5'Four _ -> NN.four
  D0'5'Five _ -> NN.five

dD0'9 :: D0'9 t a -> NonNegative
dD0'9 x = case x of
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

dD0'1'Opt :: D0'1'Opt t a -> NonNegative
dD0'1'Opt (D0'1'Opt m) = case m of
  Nothing -> NN.zero
  Just d -> dD0'1 d

dN0'19 :: N0'19 t a -> NonNegative
dN0'19 (N0'19 d1 d0)
  = (dD0'1'Opt d1)
  `NN.add` (dD0'9 d0)

dN20'23 :: N20'23 t a -> Positive
dN20'23 (N20'23 _ d3) = case NN.c'Positive'NonNegative . dD0'3 $ d3 of
  Nothing -> twenty
  Just p -> twenty `Pos.add` p
  where
    twenty = Pos.ten `Pos.mult` Pos.two

dHours :: Hours t a -> NonNegative
dHours (Hours'N0'19 n) = dN0'19 n
dHours (Hours'N20'23 n) = NN.c'NonNegative'Positive . dN20'23 $ n

dN0'59 :: N0'59 t a -> NonNegative
dN0'59 (N0'59 d1 d0)
  = (dD0'5 d1 `NN.mult` NN.ten)
  `NN.add` (dD0'9 d0)

dMinutes :: Minutes t a -> NonNegative
dMinutes (Minutes n) = dN0'59 n

dSeconds :: Seconds t a -> NonNegative
dSeconds (Seconds n) = dN0'59 n

dColonSeconds :: ColonSeconds t a -> NonNegative
dColonSeconds (ColonSeconds _ s) = dSeconds s

dColonSeconds'Opt :: ColonSeconds'Opt t a -> NonNegative
dColonSeconds'Opt (ColonSeconds'Opt m) = case m of
  Nothing -> NN.zero
  Just c -> dColonSeconds c

dTime :: Time t a -> Time.TimeOfDay
dTime (Time h _ m cs) = Time.TimeOfDay h' m' s'
  where
    h' = conv . dHours $ h
    m' = conv . dMinutes $ m
    s' = fromIntegral . conv . dColonSeconds'Opt $ cs
    conv = c'Int'Integer . NN.c'Integer'NonNegative

dWhitesTime :: WhitesTime t a -> Time.TimeOfDay
dWhitesTime (WhitesTime _ t) = dTime t

dWhitesTime'Opt :: WhitesTime'Opt t a -> Time.TimeOfDay
dWhitesTime'Opt (WhitesTime'Opt m) = case m of
  Nothing -> Time.midnight
  Just t -> dWhitesTime t

dPluMin :: Num n => PluMin t a -> n -> n
dPluMin (PluMin'Plus _) = id
dPluMin (PluMin'Minus _) = negate

dPluMin'Opt :: Num n => PluMin'Opt t a -> n -> n
dPluMin'Opt (PluMin'Opt m) = case m of
  Nothing -> id
  Just a -> dPluMin a

dZoneHrsMins :: ZoneHrsMins t a -> Time.TimeZone
dZoneHrsMins (ZoneHrsMins pm d3 d2 d1 d0) = Time.TimeZone mins False ""
  where
    mins = dPluMin pm . c'Int'Integer . NN.c'Integer'NonNegative
      $ (dD0'2 d3 `raise` NN.three)
      `NN.add` (dD0'3 d2 `raise` NN.two)
      `NN.add` (dD0'9 d1 `raise` NN.one)
      `NN.add` (dD0'9 d0)
    raise b p = b `NN.mult` (NN.ten `NN.pow` p)

dZone :: Zone t a -> Time.TimeZone
dZone (Zone _ z) = dZoneHrsMins z

dWhitesZone :: WhitesZone t a -> Time.TimeZone
dWhitesZone (WhitesZone _ z) = dZone z

dWhitesZone'Opt :: WhitesZone'Opt t a -> Time.TimeZone
dWhitesZone'Opt (WhitesZone'Opt m) = case m of
  Nothing -> Time.utc
  Just z -> dWhitesZone z

-- # Strings
dUnquotedStringNonDigitChar
  :: UnquotedStringNonDigitChar t a
  -> t
dUnquotedStringNonDigitChar (UnquotedStringNonDigitChar (c, _)) = c

dUnquotedStringNonDigitChar'Plus
  :: UnquotedStringNonDigitChar'Plus t a
  -> NonEmpty t
dUnquotedStringNonDigitChar'Plus (UnquotedStringNonDigitChar'Plus ne)
  = fmap dUnquotedStringNonDigitChar ne

dUnquotedCommodity
  :: UnquotedCommodity Char a
  -> Commodity.Commodity
dUnquotedCommodity (UnquotedCommodity p)
  = X.pack
  . toList
  . Pinchot.flatten
  . dUnquotedStringNonDigitChar'Plus
  $ p

dNonEscapedChar
  :: NonEscapedChar t a
  -> t
dNonEscapedChar (NonEscapedChar (t, _)) = t

dEscPayload
  :: EscPayload t a
  -> Maybe Char
dEscPayload x = case x of
  EscPayload'Backslash _ -> Just '\\'
  EscPayload'Newline _ -> Just '\n'
  EscPayload'DoubleQuote _ -> Just '"'
  EscPayload'Gap _ -> Nothing

dEscSeq :: EscSeq t a -> Maybe Char
dEscSeq (EscSeq _ p) = dEscPayload p

dQuotedChar :: QuotedChar Char a -> Maybe Char
dQuotedChar x = case x of
  QuotedChar'NonEscapedChar c -> Just $ dNonEscapedChar c
  QuotedChar'EscSeq c -> dEscSeq c

dQuotedChar'Star :: QuotedChar'Star Char a -> Seq Char
dQuotedChar'Star (QuotedChar'Star sq)
  = mapMaybe dQuotedChar sq

dQuotedString :: QuotedString Char a -> Seq Char
dQuotedString (QuotedString _ q _) = dQuotedChar'Star q

dQuotedCommodity :: QuotedCommodity Char a -> Commodity.Commodity
dQuotedCommodity (QuotedCommodity q)
  = X.pack
  . toList
  . dQuotedString
  $ q

dCommodity :: Commodity Char a -> Commodity.Commodity
dCommodity x = case x of
  Commodity'UnquotedCommodity c -> dUnquotedCommodity c
  Commodity'QuotedCommodity c -> dQuotedCommodity c

-- # Dates

dYear :: Year t a -> Int
dYear (Year d3 d2 d1 d0)
  = conv d3 * 1000 + conv d2 * 100
    + conv d1 * 10 + conv d0
  where
    conv = c'Int'Integer . NN.c'Integer'NonNegative . dD0'9

dMod4 :: Mod4 t a -> Int
dMod4 x = case x of
  { L04 _ _ -> 4; L08 _ _ -> 8; L12 _ _ -> 12; L16 _ _ -> 16; L20 _ _ -> 20;
    L24 _ _ -> 24; L28 _ _ -> 28; L32 _ _ -> 32; L36 _ _ -> 36;
    L40 _ _ -> 40; L44 _ _ -> 44; L48 _ _ -> 48; L52 _ _ -> 52;
    L56 _ _ -> 56; L60 _ _ -> 60; L64 _ _ -> 64; L68 _ _ -> 68;
    L72 _ _ -> 72; L76 _ _ -> 76; L80 _ _ -> 80; L84 _ _ -> 84;
    L88 _ _ -> 88; L92 _ _ -> 92; L96 _ _ -> 96 }

dCenturyLeapYear :: CenturyLeapYear t a -> Int
dCenturyLeapYear (LeapYear0 _ _ _ _) = 0
dCenturyLeapYear (LeapYearMod4 m4 _ _) = dMod4 m4 * 100

dNonCenturyLeapYear :: NonCenturyLeapYear t a -> Int
dNonCenturyLeapYear (NonCenturyLeapYear d2 d1 m4)
  = conv d2 * 1000 + conv d1 * 100 + dMod4 m4
  where
    conv = c'Int'Integer . NN.c'Integer'NonNegative . dD0'9

dLeapYear :: LeapYear t a -> Int
dLeapYear (LeapYear'CenturyLeapYear x) = dCenturyLeapYear x
dLeapYear (LeapYear'NonCenturyLeapYear x) = dNonCenturyLeapYear x

dDays28 :: Days28 t a -> Positive
dDays28 x = case x of
  D28'1to9 _ d -> dD1'9 d
  D28'10to19 _ d -> case NN.c'Positive'NonNegative (dD0'9 d) of
    Nothing -> Pos.ten
    Just p -> Pos.ten `Pos.add` p
  D28'20to28 _ d -> case NN.c'Positive'NonNegative (dD0'8 d) of
    Nothing -> Pos.ten `Pos.mult` Pos.two
    Just p -> (Pos.ten `Pos.mult` Pos.two) `Pos.add` p

dDays30 :: Days30 t a -> Positive
dDays30 (D30'28 d28) = dDays28 d28
dDays30 (D30'29 _ _) = (Pos.ten `Pos.mult` Pos.two) `Pos.add` Pos.nine
dDays30 (D30'30 _ _) = (Pos.ten `Pos.mult` Pos.three)

dDays31 :: Days31 t a -> Positive
dDays31 (D31'30 d30) = dDays30 d30
dDays31 (D31'31 _ _) = (Pos.ten `Pos.mult` Pos.three) `Pos.add` Pos.one

dMonthDay
  :: MonthDay t a
  -> (Positive, Positive)
  -- ^ Positive for the month (1 is January, 12 is December) and for
  -- day
dMonthDay x = case x of
  Jan _ _ _ d31 -> (Pos.one, dDays31 d31)
  Feb _ _ _ d28 -> (Pos.two, dDays28 d28)
  Mar _ _ _ d31 -> (Pos.three, dDays31 d31)
  Apr _ _ _ d30 -> (Pos.four, dDays30 d30)
  May _ _ _ d31 -> (Pos.five, dDays31 d31)
  Jun _ _ _ d30 -> (Pos.six, dDays30 d30)
  Jul _ _ _ d31 -> (Pos.seven, dDays31 d31)
  Aug _ _ _ d31 -> (Pos.eight, dDays31 d31)
  Sep _ _ _ d30 -> (Pos.nine, dDays30 d30)
  Oct _ _ _ d31 -> (Pos.ten, dDays31 d31)
  Nov _ _ _ d30 -> (Pos.ten `Pos.add` Pos.one, dDays30 d30)
  Dec _ _ _ d31 -> (Pos.ten `Pos.add` Pos.two, dDays31 d31)

dDate :: Date t a -> Time.Day
dDate x = case x of
  Date'NonLeapDay (NonLeapDay yr _ md) ->
    Time.fromGregorian (fromIntegral y) (conv m) (conv d)
    where
      y = dYear yr
      (m, d) = dMonthDay md
      conv = c'Int'Integer . Pos.c'Integer'Positive
  Date'LeapDay (LeapDay yr _ _ _ _ _ _) ->
    Time.fromGregorian (fromIntegral y) m d
    where
      y = dLeapYear yr
      m = 2
      d = 29

-- # Numbers

dNilUngroupedRadCom :: NilUngroupedRadCom t a -> DecZero
dNilUngroupedRadCom = Exponential () . e'NilUngroupedRadCom
  where
    e'NilUngroupedRadCom :: NilUngroupedRadCom t a -> NonNegative
    e'NilUngroupedRadCom (NUZeroRadCom _ (RadixZeroesRadCom'Opt Nothing))
      = NN.zero
    e'NilUngroupedRadCom (NUZeroRadCom _ (RadixZeroesRadCom'Opt
      (Just (RadixZeroesRadCom _ (Zero'Star sq))))) = length sq
    e'NilUngroupedRadCom (NURadixRadCom _ _z1 (Zero'Star zs))
      = NN.one `NN.add` (length zs)

dNilUngroupedRadPer :: NilUngroupedRadPer t a -> DecZero
dNilUngroupedRadPer = Exponential () . e'NilUngroupedRadPer
  where
    e'NilUngroupedRadPer :: NilUngroupedRadPer t a -> NonNegative
    e'NilUngroupedRadPer (NUZeroRadPer _ (RadixZeroesRadPer'Opt Nothing))
      = NN.zero
    e'NilUngroupedRadPer (NUZeroRadPer _ (RadixZeroesRadPer'Opt
      (Just (RadixZeroesRadPer _ (Zero'Star sq))))) = length sq
    e'NilUngroupedRadPer (NURadixRadPer _ _z1 (Zero'Star zs))
      = NN.one `NN.add` (length zs)

dNilGroupedRadCom :: NilGroupedRadCom t a -> DecZero
dNilGroupedRadCom = Exponential () . e'NilGroupedRadCom
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

dNilGroupedRadPer :: NilGroupedRadPer t a -> DecZero
dNilGroupedRadPer = Exponential () . e'NilGroupedRadPer
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

dNilRadCom :: NilRadCom t a -> DecZero
dNilRadCom x = case x of
  NilRadCom'NilUngroupedRadCom c -> dNilUngroupedRadCom c
  NilRadCom'NilGroupedRadCom c -> dNilGroupedRadCom c

dNilRadPer :: NilRadPer t a -> DecZero
dNilRadPer x = case x of
  NilRadPer'NilUngroupedRadPer c -> dNilUngroupedRadPer c
  NilRadPer'NilGroupedRadPer c -> dNilGroupedRadPer c

dNeutral :: Neutral t a -> DecZero
dNeutral (NeuCom _ n) = dNilRadCom n
dNeutral (NeuPer n) = dNilRadPer n

dRadixComDigits :: RadixComDigits t a -> NonNegative
dRadixComDigits (RadixComDigits _ (D0'9'Star sq)) = length sq

dRadixPerDigits :: RadixPerDigits t a -> NonNegative
dRadixPerDigits (RadixPerDigits _ (D0'9'Star sq)) = length sq

dRadixComDigits'Opt :: RadixComDigits'Opt t a -> NonNegative
dRadixComDigits'Opt (RadixComDigits'Opt may)
  = maybe NN.zero dRadixComDigits may

dRadixPerDigits'Opt :: RadixPerDigits'Opt t a -> NonNegative
dRadixPerDigits'Opt (RadixPerDigits'Opt may)
  = maybe NN.zero dRadixPerDigits may

dBrimUngroupedRadCom :: BrimUngroupedRadCom t a -> DecPositive
dBrimUngroupedRadCom (BUGreaterThanOneRadCom nv (D0'9'Star ds1)
  (RadixComDigits'Opt Nothing))
  = Exponential (novDecsToPositive nv ds1) NN.zero

dBrimUngroupedRadCom (BUGreaterThanOneRadCom nv (D0'9'Star ds1)
  (RadixComDigits'Opt (Just (RadixComDigits _ (D0'9'Star ds2)))))
  = Exponential (novDecsToPositive nv (ds1 <> ds2))
                (length ds2)

dBrimUngroupedRadCom
  (BULessThanOneRadCom _ _ (Zero'Star zs1) nv (D0'9'Star ds))
  = Exponential (novDecsToPositive nv ds)
                (NN.one `NN.add` (length zs1) `NN.add` (length ds))

dBrimUngroupedRadPer :: BrimUngroupedRadPer t a -> DecPositive
dBrimUngroupedRadPer (BUGreaterThanOneRadPer nv (D0'9'Star ds1)
  (RadixPerDigits'Opt Nothing))
  = Exponential (novDecsToPositive nv ds1) NN.zero

dBrimUngroupedRadPer (BUGreaterThanOneRadPer nv (D0'9'Star ds1)
  (RadixPerDigits'Opt (Just (RadixPerDigits _ (D0'9'Star ds2)))))
  = Exponential (novDecsToPositive nv (ds1 <> ds2))
                (length ds2)

dBrimUngroupedRadPer
  (BULessThanOneRadPer _ _ (Zero'Star zs1) nv (D0'9'Star ds))
  = Exponential (novDecsToPositive nv ds)
                (NN.one `NN.add` (length zs1) `NN.add` (length ds))

dBrimGroupedRadCom :: BrimGroupedRadCom t a -> DecPositive
dBrimGroupedRadCom
  = dBrimUngroupedRadCom . ungroupBrimGroupedRadCom

dBrimGroupedRadPer :: BrimGroupedRadPer t a -> DecPositive
dBrimGroupedRadPer
  = dBrimUngroupedRadPer . ungroupBrimGroupedRadPer

dBrimRadCom :: BrimRadCom t a -> DecPositive
dBrimRadCom
  = dBrimUngroupedRadCom . ungroupBrimRadCom

dBrimRadPer :: BrimRadPer t a -> DecPositive
dBrimRadPer
  = dBrimUngroupedRadPer . ungroupBrimRadPer

dNonNeutral :: NonNeutral t a -> DecPositive
dNonNeutral (NonNeutralRadCom _ b) = dBrimRadCom b
dNonNeutral (NonNeutralRadPer b) = dBrimRadPer b

dPluMinNonNeutral :: PluMinNonNeutral t a -> DecNonZero
dPluMinNonNeutral (PluMinNonNeutral pm _ nn)
  = c'DecNonZero'DecPositive pole (dNonNeutral nn)
  where
    pole = case pm of
      PluMin'Plus _ -> positive
      PluMin'Minus _ -> negative

dExchNonNeu :: ExchNonNeu t a -> DecNonZero
dExchNonNeu x = case x of
  ExchNonNeu'PluMinNonNeutral c -> dPluMinNonNeutral c
  ExchNonNeu'NonNeutral c -> c'DecNonZero'DecPositive positive
    . dNonNeutral $ c

dExch :: Exch t a -> Decimal
dExch x = case x of
  Exch'ExchNonNeu n -> fmap c'Integer'NonZero . dExchNonNeu $ n
  Exch'Neutral n -> fmap (const 0) . dNeutral $ n

dCyExch :: CyExch Char a -> (Commodity.Commodity, Decimal)
dCyExch (CyExch c _ e) = (dCommodity c, dExch e)

dExchCy :: ExchCy Char a -> (Commodity.Commodity, Decimal)
dExchCy (ExchCy e _ c) = (dCommodity c, dExch e)

dJanus :: Janus Char a -> (Commodity.Commodity, Decimal)
dJanus x = case x of
  Janus'CyExch c -> dCyExch c
  Janus'ExchCy c -> dExchCy c

type TxnParts = (Loc, Seq Tree.Tree, Seq (Loc, Trio.Trio, Seq Tree.Tree))

dPrice :: Price Char Loc -> PriceParts Loc
dPrice p = PriceParts loc zt from to exch
  where
    loc = Lens.view (Lens.to _r'Price'0'AtSign . Lens._Wrapped' . Lens._2) p
    zt = Time.ZonedTime lt tz
    tod = dWhitesTime'Opt . _r'Price'3'WhitesTime'Opt $ p
    day = dDate . _r'Price'2'Date $ p
    tz = dWhitesZone'Opt . _r'Price'4'WhitesZone'Opt $ p
    lt = Time.LocalTime day tod
    from = dCommodity . _r'Price'6'Commodity $ p
    (to, exch) = dJanus . _r'Price'8'Janus $ p

dUnquotedStringNonFirstChar
  :: UnquotedStringNonFirstChar t a
  -> t
dUnquotedStringNonFirstChar x = case x of
  UnquotedStringNonFirstChar'UnquotedStringNonDigitChar c ->
    dUnquotedStringNonDigitChar c
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

dUnquotedStringNonFirstChar'Star
  :: UnquotedStringNonFirstChar'Star t a
  -> Seq t
dUnquotedStringNonFirstChar'Star (UnquotedStringNonFirstChar'Star sq)
  = fmap dUnquotedStringNonFirstChar sq

dUnquotedString
  :: UnquotedString t a
  -> Seq t
dUnquotedString (UnquotedString ds c1 cs)
  = digits ds <> (dUnquotedStringNonDigitChar c1 `Lens.cons`
      dUnquotedStringNonFirstChar'Star cs)
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

dWholeNonZero :: WholeNonZero t a -> NonZero
dWholeNonZero (WholeNonZero (PluMin'Opt mayPm) d1 (D0'9'Star ds))
  = flip . NZ.c'NonZero'Positive $ novDecsToPositive d1 ds
  where
    flip = case mayPm of
      Nothing -> id
      Just pm -> case pm of
        PluMin'Plus _ -> id
        PluMin'Minus _ -> NZ.negate

dWholeAny :: WholeAny t a -> Integer
dWholeAny x = case x of
  WholeAny'Zero _ -> 0
  WholeAny'WholeNonZero w -> NZ.c'Integer'NonZero . dWholeNonZero $ w

dScalar :: Scalar Char a -> Scalar.Scalar
dScalar x = case x of
  Scalar'UnquotedString a -> Scalar.SText . X.pack . toList
    . dUnquotedString $ a
  Scalar'QuotedString a -> Scalar.SText . X.pack . toList
    . dQuotedString $ a
  Scalar'Date a -> Scalar.SDay . dDate $ a
  Scalar'Time a -> Scalar.STime . dTime $ a
  Scalar'Zone a -> Scalar.SZone . Time.timeZoneMinutes . dZone $ a
  Scalar'WholeAny a -> Scalar.SInteger . dWholeAny $ a
  Scalar'Label (Label _ us) -> Scalar.SLabel . X.pack . toList
    . dUnquotedString $ us

dWhitesScalar :: WhitesScalar Char a -> Scalar.Scalar
dWhitesScalar (WhitesScalar _ s) = dScalar s

dWhitesScalar'Opt :: WhitesScalar'Opt Char a -> Maybe Scalar.Scalar
dWhitesScalar'Opt (WhitesScalar'Opt m) = fmap dWhitesScalar m

locToTree :: Loc -> Tree.Tree
locToTree l = Tree.Tree scalarLoc . Seq.fromList $
  [ intTree "line" (Pinchot._line l)
  , intTree "column" (Pinchot._col l)
  , intTree "position" (Pinchot._pos l)
  ]
  where
    scalarLoc = Just $ Scalar.SText "location"
    intTree txt int = Tree.Tree scalarTxt . Seq.singleton $ treeInt
      where
        scalarTxt = Just $ Scalar.SText txt
        treeInt = Tree.Tree
          (Just $ Scalar.SInteger (fromIntegral int)) Seq.empty

dNextTree :: NextTree Char Loc -> Tree.Tree
dNextTree (NextTree _ _ _ t) = dTree t

dTree :: Tree Char Loc -> Tree.Tree
dTree x = Tree.Tree sc (loc `Lens.cons` children)
  where
    loc = locToTree . snd . Pinchot._front . t'Tree $ x
    (sc, children) = case x of
      Tree'ScalarMaybeForest s -> (Just scalar, trees)
        where
          (scalar, trees) = dScalarMaybeForest s
      Tree'ForestMaybeScalar s -> (mayScalar, Pinchot.flatten trees)
        where
          (trees, mayScalar) = dForestMaybeScalar s

dForestMaybeScalar
  :: ForestMaybeScalar Char Loc
  -> (NonEmpty Tree.Tree, Maybe Scalar.Scalar)
dForestMaybeScalar (ForestMaybeScalar bf sc)
  = (dBracketedForest bf, dWhitesScalar'Opt sc)

dBracketedForest
  :: BracketedForest Char Loc
  -> NonEmpty Tree.Tree
dBracketedForest (BracketedForest _ _ forest _ _)
  = dForest forest

dForest
  :: Forest Char Loc
  -> NonEmpty Tree.Tree
dForest (Forest t1 ts)
  = Pinchot.NonEmpty (dTree t1) (dNextTree'Star ts)

dNextTree'Star
  :: NextTree'Star Char Loc
  -> Seq Tree.Tree
dNextTree'Star (NextTree'Star sq)
  = fmap dNextTree sq

dScalarMaybeForest
  :: ScalarMaybeForest Char Loc
  -> (Scalar.Scalar, Seq Tree.Tree)
dScalarMaybeForest (ScalarMaybeForest sc wbf)
  = (dScalar sc, dWhitesBracketedForest'Opt wbf)

dWhitesBracketedForest'Opt
  :: WhitesBracketedForest'Opt Char Loc
  -> Seq Tree.Tree
dWhitesBracketedForest'Opt (WhitesBracketedForest'Opt may) = case may of
  Nothing -> Seq.empty
  Just wbf -> Pinchot.flatten . dWhitesBracketedForest $ wbf

dWhitesBracketedForest
  :: WhitesBracketedForest Char Loc
  -> NonEmpty Tree.Tree
dWhitesBracketedForest (WhitesBracketedForest _ bf)
  = dBracketedForest bf

dTopLine :: TopLine Char Loc -> NonEmpty Tree.Tree
dTopLine (TopLine f) = dForest f

dDebitCredit :: DebitCredit t a -> Pole
dDebitCredit x = case x of
  DebitCredit'Debit _ -> debit
  DebitCredit'Credit _ -> credit

dT_DebitCredit :: T_DebitCredit t a -> Trio.Trio
dT_DebitCredit (T_DebitCredit x) = Trio.S $ dDebitCredit x

dT_DebitCredit_Commodity :: T_DebitCredit_Commodity Char a -> Trio.Trio
dT_DebitCredit_Commodity (T_DebitCredit_Commodity dc _ cy)
  = Trio.SC (dDebitCredit dc) (dCommodity cy)

dT_DebitCredit_NonNeutral
  :: T_DebitCredit_NonNeutral Char a -> Trio.Trio
dT_DebitCredit_NonNeutral dcnn = Trio.Q repAnyRadix
  where
    T_DebitCredit_NonNeutral dc _ nn2 = fmap (const ()) dcnn
    p = dDebitCredit dc
    repAnyRadix = case nn2 of
      NonNeutralRadCom _ brimRadCom ->
        Left $ Extreme  (Polarized brimRadCom p)
      NonNeutralRadPer brimRadPer ->
        Right $ Extreme (Polarized brimRadPer p)

dT_DebitCredit_Commodity_NonNeutral
  :: T_DebitCredit_Commodity_NonNeutral Char a
  -> Trio.Trio
dT_DebitCredit_Commodity_NonNeutral dcnn = Trio.QC repAnyRadix cy arrangement
  where
    T_DebitCredit_Commodity_NonNeutral dc0 _ c2 w3 nn4 = fmap (const ()) dcnn
    cy = dCommodity c2
    p = dDebitCredit dc0
    isSpace = not . Seq.null . t'White'Star $ w3
    repAnyRadix = case nn4 of
        NonNeutralRadCom _ brimRadCom ->
          Left $ Extreme  (Polarized brimRadCom p)
        NonNeutralRadPer brimRadPer ->
          Right $ Extreme (Polarized brimRadPer p)
    arrangement = Arrangement CommodityOnLeft isSpace

dT_DebitCredit_NonNeutral_Commodity
  :: T_DebitCredit_NonNeutral_Commodity Char a
  -> Trio.Trio
dT_DebitCredit_NonNeutral_Commodity dcnn = Trio.QC repAnyRadix cy arrangement
  where
    T_DebitCredit_NonNeutral_Commodity dc0 _ nn2 w3 c4 = fmap (const ()) dcnn
    cy = dCommodity c4
    p = dDebitCredit dc0
    isSpace = not . Seq.null . t'White'Star $ w3
    repAnyRadix = case nn2 of
        NonNeutralRadCom _ brimRadCom ->
          Left $ Extreme  (Polarized brimRadCom p)
        NonNeutralRadPer brimRadPer ->
          Right $ Extreme (Polarized brimRadPer p)
    arrangement = Arrangement CommodityOnRight isSpace

dT_Commodity :: T_Commodity Char a -> Trio.Trio
dT_Commodity (T_Commodity c) = Trio.C (dCommodity c)

dT_Commodity_Neutral :: T_Commodity_Neutral Char a -> Trio.Trio
dT_Commodity_Neutral cn
  = Trio.NC nilAnyRadix cy (Arrangement CommodityOnLeft isSpace)
  where
    T_Commodity_Neutral cy0 w1 n2 = fmap (const ()) cn
    cy = dCommodity cy0
    isSpace = not . Seq.null . t'White'Star $ w1
    nilAnyRadix = case n2 of
      NeuCom _ nilRadCom -> Left nilRadCom
      NeuPer nilRadPer -> Right nilRadPer

dT_Neutral_Commodity :: T_Neutral_Commodity Char a -> Trio.Trio
dT_Neutral_Commodity cn
  = Trio.NC nilAnyRadix cy (Arrangement CommodityOnRight isSpace)
  where
    T_Neutral_Commodity n0 w1 cy2 = fmap (const ()) cn
    cy = dCommodity cy2
    isSpace = not . Seq.null . t'White'Star $ w1
    nilAnyRadix = case n0 of
      NeuCom _ nilRadCom -> Left nilRadCom
      NeuPer nilRadPer -> Right nilRadPer

dT_Commodity_NonNeutral :: T_Commodity_NonNeutral Char a -> Trio.Trio
dT_Commodity_NonNeutral cnn
  = Trio.UC brimScalarAnyRadix cy (Arrangement CommodityOnLeft isSpace)
  where
    T_Commodity_NonNeutral cy0 w1 n2 = fmap (const ()) cnn
    cy = dCommodity cy0
    isSpace = not . Seq.null . t'White'Star $ w1
    brimScalarAnyRadix = case n2 of
      NonNeutralRadCom _ nilRadCom -> Left nilRadCom
      NonNeutralRadPer nilRadPer -> Right nilRadPer

dT_NonNeutral_Commodity :: T_NonNeutral_Commodity Char a -> Trio.Trio
dT_NonNeutral_Commodity cnn
  = Trio.UC brimScalarAnyRadix cy (Arrangement CommodityOnRight isSpace)
  where
    T_NonNeutral_Commodity n0 w1 cy2 = fmap (const ()) cnn
    cy = dCommodity cy2
    isSpace = not . Seq.null . t'White'Star $ w1
    brimScalarAnyRadix = case n0 of
      NonNeutralRadCom _ nilRadCom -> Left nilRadCom
      NonNeutralRadPer nilRadPer -> Right nilRadPer

dT_Neutral :: T_Neutral Char a -> Trio.Trio
dT_Neutral (T_Neutral n0) = Trio.UU nilAnyRadix
  where
    nilAnyRadix = case fmap (const ()) n0 of
      NeuCom _ nilRadCom -> Left nilRadCom
      NeuPer nilRadPer -> Right nilRadPer

dT_NonNeutral :: T_NonNeutral Char a -> Trio.Trio
dT_NonNeutral (T_NonNeutral n0) = Trio.US brimScalarAnyRadix
  where
    brimScalarAnyRadix = case fmap (const ()) n0 of
        NonNeutralRadCom _ brimRadCom -> Left brimRadCom
        NonNeutralRadPer brimRadPer -> Right brimRadPer

dTrio :: Trio Char a -> Trio.Trio
dTrio x = case x of
  Trio'T_DebitCredit a -> dT_DebitCredit a
  Trio'T_DebitCredit_Commodity a -> dT_DebitCredit_Commodity a
  Trio'T_DebitCredit_NonNeutral a -> dT_DebitCredit_NonNeutral a
  Trio'T_DebitCredit_Commodity_NonNeutral a ->
    dT_DebitCredit_Commodity_NonNeutral a
  Trio'T_DebitCredit_NonNeutral_Commodity a ->
    dT_DebitCredit_NonNeutral_Commodity a
  Trio'T_Commodity a -> dT_Commodity a
  Trio'T_Commodity_Neutral a -> dT_Commodity_Neutral a
  Trio'T_Neutral_Commodity a -> dT_Neutral_Commodity a
  Trio'T_Commodity_NonNeutral a -> dT_Commodity_NonNeutral a
  Trio'T_NonNeutral_Commodity a -> dT_NonNeutral_Commodity a
  Trio'T_Neutral a -> dT_Neutral a
  Trio'T_NonNeutral a -> dT_NonNeutral a

dTrioMaybeForest
  :: TrioMaybeForest Char Loc
  -> (Trio.Trio, Seq Tree.Tree)
dTrioMaybeForest (TrioMaybeForest t wbf)
  = (dTrio t, dWhitesBracketedForest'Opt wbf)

dPosting
  :: Posting Char Loc
  -> (Loc, Trio.Trio, Seq Tree.Tree)
dPosting x = case x of
  Posting'TrioMaybeForest tmf -> (\(a, b) -> (loc, a, b))
    $ dTrioMaybeForest tmf
  Posting'BracketedForest bf -> (loc, Trio.E, Pinchot.flatten ts)
    where
      ts = dBracketedForest bf
  where
    loc = snd . Pinchot._front . t'Posting $ x

dNextPosting
  :: NextPosting Char Loc
  -> (Loc, Trio.Trio, Seq Tree.Tree)
dNextPosting (NextPosting _ _ _ p) = dPosting p

dNextPosting'Star
  :: NextPosting'Star Char Loc
  -> Seq (Loc, Trio.Trio, Seq Tree.Tree)
dNextPosting'Star (NextPosting'Star x) = fmap dNextPosting x

dPostingList
  :: PostingList Char Loc
  -> NonEmpty (Loc, Trio.Trio, Seq Tree.Tree)
dPostingList (PostingList _ p1 ps)
  = Pinchot.NonEmpty (dPosting p1) (dNextPosting'Star ps)

dPostingList'Opt
  :: PostingList'Opt Char Loc
  -> Seq (Loc, Trio.Trio, Seq Tree.Tree)
dPostingList'Opt (PostingList'Opt m) = case m of
  Nothing -> Seq.empty
  Just l -> Pinchot.flatten . dPostingList $ l

dPostings
  :: Postings Char Loc
  -> (Loc, Seq (Loc, Trio.Trio, Seq Tree.Tree))
dPostings (Postings oc pl _ _) = (loc, dPostingList'Opt pl)
  where
    OpenCurly (_, loc) = oc

dWhitesPostings
  :: WhitesPostings Char Loc
  -> (Loc, Seq (Loc, Trio.Trio, Seq Tree.Tree))
dWhitesPostings (WhitesPostings _ p) = dPostings p

dWhitesPostings'Opt
  :: WhitesPostings'Opt Char Loc
  -> Maybe (Loc, Seq (Loc, Trio.Trio, Seq Tree.Tree))
dWhitesPostings'Opt (WhitesPostings'Opt m)
  = fmap dWhitesPostings m

dTopLineMaybePostings
  :: TopLineMaybePostings Char Loc
  -> (Loc, NonEmpty Tree.Tree, Seq (Loc, Trio.Trio, Seq Tree.Tree))
dTopLineMaybePostings tlp@(TopLineMaybePostings t w)
  = (loc, dTopLine t, trips)
  where
    loc = snd . _front . t'TopLineMaybePostings $ tlp
    trips = case dWhitesPostings'Opt w of
      Nothing -> Seq.empty
      Just (_, t) -> t

dTransaction
  :: Transaction Char Loc
  -> (Loc, Seq Tree.Tree, Seq (Loc, Trio.Trio, Seq Tree.Tree))
dTransaction x = case x of
  Transaction'TopLineMaybePostings t ->
    Lens.over Lens._2 Pinchot.flatten $ dTopLineMaybePostings t
  Transaction'Postings p -> (loc, Seq.empty, trips)
    where
      (loc, trips) = dPostings p

dFileItem
  :: FileItem Char Loc
  -> Maybe (Either (PriceParts Loc) TxnParts)
dFileItem x = case x of
  FileItem'Price p -> Just . Left $ dPrice p
  FileItem'Transaction t -> Just . Right $ dTransaction t
  FileItem'Comment _ -> Nothing

dWhitesFileItem
  :: WhitesFileItem Char Loc
  -> Maybe (Either (PriceParts Loc) TxnParts)
dWhitesFileItem (WhitesFileItem _ i) = dFileItem i

dWhitesFileItem'Star
  :: WhitesFileItem'Star Char Loc
  -> Seq (Either (PriceParts Loc) TxnParts)
dWhitesFileItem'Star (WhitesFileItem'Star sq)
  = catMaybes . fmap dWhitesFileItem $ sq

dWholeFile
  :: WholeFile Char Loc
  -> Seq (Either (PriceParts Loc) TxnParts)
dWholeFile (WholeFile x _) = dWhitesFileItem'Star x

dNilOrBrimRadCom
  :: NilOrBrimRadCom t a
  -> DecUnsigned
dNilOrBrimRadCom x = case x of
  NilOrBrimRadCom'NilRadCom nrc -> fmap (const NN.zero) (dNilRadCom nrc)
  NilOrBrimRadCom'BrimRadCom brc -> fmap NN.c'NonNegative'Positive
    (dBrimRadCom brc)

dNilOrBrimRadPer
  :: NilOrBrimRadPer t a
  -> DecUnsigned
dNilOrBrimRadPer x = case x of
  NilOrBrimRadPer'NilRadPer nrc -> fmap (const NN.zero) (dNilRadPer nrc)
  NilOrBrimRadPer'BrimRadPer brc -> fmap NN.c'NonNegative'Positive
    (dBrimRadPer brc)

e'NilUngroupedRadCom :: NilUngroupedRadCom t a -> NonNegative
e'NilUngroupedRadCom (NUZeroRadCom _ (RadixZeroesRadCom'Opt Nothing)) = zero
e'NilUngroupedRadCom (NUZeroRadCom _ (RadixZeroesRadCom'Opt
  (Just (RadixZeroesRadCom _ (Zero'Star sq))))) = length sq
e'NilUngroupedRadCom (NURadixRadCom _ _z1 (Zero'Star zs))
  = one `NN.add` (length zs)

e'NilUngroupedRadPer :: NilUngroupedRadPer t a -> NonNegative
e'NilUngroupedRadPer (NUZeroRadPer _ (RadixZeroesRadPer'Opt Nothing)) = zero
e'NilUngroupedRadPer (NUZeroRadPer _ (RadixZeroesRadPer'Opt
  (Just (RadixZeroesRadPer _ (Zero'Star sq))))) = length sq
e'NilUngroupedRadPer (NURadixRadPer _ _z1 (Zero'Star zs))
  = one `NN.add` (length zs)

c'DecZero'NilUngroupedRadCom :: NilUngroupedRadCom t a -> DecZero
c'DecZero'NilUngroupedRadCom = Exponential () . e'NilUngroupedRadCom

c'DecZero'NilUngroupedRadPer :: NilUngroupedRadPer t a -> DecZero
c'DecZero'NilUngroupedRadPer = Exponential () . e'NilUngroupedRadPer

e'NilGroupedRadCom :: NilGroupedRadCom t a -> NonNegative
e'NilGroupedRadCom (NilGroupedRadCom _zMay _rdx _z1 zs1 zss)
  = one `NN.add` zeroes1 `NN.add` zeroesRest
  where
    zeroes1 = let Zero'Star zs = zs1 in length zs
    zeroesRest = addGroup g1 (foldr addGroup zero gs)
      where
        ZeroGroupRadCom'Plus (Pinchot.NonEmpty g1 gs) = zss
        addGroup (ZeroGroupRadCom _ _zero1 (Zero'Star zeros)) acc
          = one `NN.add` length zeros `NN.add` acc

e'NilGroupedRadPer :: NilGroupedRadPer t a -> NonNegative
e'NilGroupedRadPer (NilGroupedRadPer _zMay _rdx _z1 zs1 zss)
  = one `NN.add` zeroes1 `NN.add` zeroesRest
  where
    zeroes1 = let Zero'Star zs = zs1 in length zs
    zeroesRest = addGroup g1 (foldr addGroup zero gs)
      where
        ZeroGroupRadPer'Plus (Pinchot.NonEmpty g1 gs) = zss
        addGroup (ZeroGroupRadPer _ _zero1 (Zero'Star zeros)) acc
          = one `NN.add` length zeros `NN.add` acc

c'DecZero'NilGroupedRadCom :: NilGroupedRadCom t a -> DecZero
c'DecZero'NilGroupedRadCom = Exponential () . e'NilGroupedRadCom

c'DecZero'NilGroupedRadPer :: NilGroupedRadPer t a -> DecZero
c'DecZero'NilGroupedRadPer = Exponential () . e'NilGroupedRadPer

e'NilRadCom :: NilRadCom t a -> NonNegative
e'NilRadCom (NilRadCom'NilUngroupedRadCom x) = e'NilUngroupedRadCom x
e'NilRadCom (NilRadCom'NilGroupedRadCom x) = e'NilGroupedRadCom x

e'NilRadPer :: NilRadPer t a -> NonNegative
e'NilRadPer (NilRadPer'NilUngroupedRadPer x) = e'NilUngroupedRadPer x
e'NilRadPer (NilRadPer'NilGroupedRadPer x) = e'NilGroupedRadPer x

c'DecZero'NilRadCom :: NilRadCom t a -> DecZero
c'DecZero'NilRadCom = Exponential () . e'NilRadCom

c'DecZero'NilRadPer :: NilRadPer t a -> DecZero
c'DecZero'NilRadPer = Exponential () . e'NilRadPer

c'DecZero'Neutral :: Neutral t a -> DecZero
c'DecZero'Neutral (NeuCom _ n) = c'DecZero'NilRadCom n
c'DecZero'Neutral (NeuPer n) = c'DecZero'NilRadPer n

e'RadixComDigits :: RadixComDigits t a -> NonNegative
e'RadixComDigits (RadixComDigits _ (D0'9'Star sq)) = length sq

e'RadixPerDigits :: RadixPerDigits t a -> NonNegative
e'RadixPerDigits (RadixPerDigits _ (D0'9'Star sq)) = length sq

e'RadixComDigits'Opt :: RadixComDigits'Opt t a -> NonNegative
e'RadixComDigits'Opt (RadixComDigits'Opt may)
  = maybe zero e'RadixComDigits may

e'RadixPerDigits'Opt :: RadixPerDigits'Opt t a -> NonNegative
e'RadixPerDigits'Opt (RadixPerDigits'Opt may)
  = maybe zero e'RadixPerDigits may

e'BrimUngroupedRadCom :: BrimUngroupedRadCom t a -> NonNegative
e'BrimUngroupedRadCom
  (BUGreaterThanOneRadCom _ _ mayRadCom) = e'RadixComDigits'Opt mayRadCom
e'BrimUngroupedRadCom
  (BULessThanOneRadCom _ _rdx (Zero'Star zs1) _d2 (D0'9'Star dss))
  = length zs1 `NN.add` one `NN.add` length dss

e'BrimUngroupedRadPer :: BrimUngroupedRadPer t a -> NonNegative
e'BrimUngroupedRadPer
  (BUGreaterThanOneRadPer _ _ mayRadPer) = e'RadixPerDigits'Opt mayRadPer
e'BrimUngroupedRadPer
  (BULessThanOneRadPer _ _rdx (Zero'Star zs1) _d2 (D0'9'Star dss))
  = length zs1 `NN.add` one `NN.add` length dss

c'DecPositive'BrimUngroupedRadCom :: BrimUngroupedRadCom t a -> DecPositive
c'DecPositive'BrimUngroupedRadCom (BUGreaterThanOneRadCom nv (D0'9'Star ds1)
  (RadixComDigits'Opt Nothing))
  = Exponential (novDecsToPositive nv ds1) zero

c'DecPositive'BrimUngroupedRadCom (BUGreaterThanOneRadCom nv (D0'9'Star ds1)
  (RadixComDigits'Opt (Just (RadixComDigits _ (D0'9'Star ds2)))))
  = Exponential (novDecsToPositive nv (ds1 <> ds2))
                (length ds2)

c'DecPositive'BrimUngroupedRadCom
  (BULessThanOneRadCom _ _ (Zero'Star zs1) nv (D0'9'Star ds))
  = Exponential (novDecsToPositive nv ds)
                (one `NN.add` (length zs1) `NN.add` (length ds))

c'DecPositive'BrimUngroupedRadPer :: BrimUngroupedRadPer t a -> DecPositive
c'DecPositive'BrimUngroupedRadPer (BUGreaterThanOneRadPer nv (D0'9'Star ds1)
  (RadixPerDigits'Opt Nothing))
  = Exponential (novDecsToPositive nv ds1) zero

c'DecPositive'BrimUngroupedRadPer (BUGreaterThanOneRadPer nv (D0'9'Star ds1)
  (RadixPerDigits'Opt (Just (RadixPerDigits _ (D0'9'Star ds2)))))
  = Exponential (novDecsToPositive nv (ds1 <> ds2))
                (length ds2)

c'DecPositive'BrimUngroupedRadPer
  (BULessThanOneRadPer _ _ (Zero'Star zs1) nv (D0'9'Star ds))
  = Exponential (novDecsToPositive nv ds)
                (one `NN.add` (length zs1) `NN.add` (length ds))

c'DecPositive'BrimGroupedRadCom :: BrimGroupedRadCom t a -> DecPositive
c'DecPositive'BrimGroupedRadCom
  = c'DecPositive'BrimUngroupedRadCom . ungroupBrimGroupedRadCom

c'DecPositive'BrimGroupedRadPer :: BrimGroupedRadPer t a -> DecPositive
c'DecPositive'BrimGroupedRadPer
  = c'DecPositive'BrimUngroupedRadPer . ungroupBrimGroupedRadPer

c'DecPositive'BrimRadCom :: BrimRadCom t a -> DecPositive
c'DecPositive'BrimRadCom
  = c'DecPositive'BrimUngroupedRadCom . ungroupBrimRadCom

c'DecPositive'BrimRadPer :: BrimRadPer t a -> DecPositive
c'DecPositive'BrimRadPer
  = c'DecPositive'BrimUngroupedRadPer . ungroupBrimRadPer

c'DecPositive'NonNeutral :: NonNeutral t a -> DecPositive
c'DecPositive'NonNeutral (NonNeutralRadCom _ b) = c'DecPositive'BrimRadCom b
c'DecPositive'NonNeutral (NonNeutralRadPer b) = c'DecPositive'BrimRadPer b

c'Decimal'RepRadCom :: RepRadCom -> Decimal
c'Decimal'RepRadCom x = case x of
  Moderate nrc -> Lens.over coefficient (const 0)
    . c'DecZero'NilRadCom $ nrc
  Extreme (Polarized brimRadCom pole) ->
      Lens.over coefficient (changeSign . Pos.c'Integer'Positive)
      . c'DecPositive'BrimRadCom
      $ brimRadCom
      where
        changeSign
          | pole == positive = id
          | otherwise = Prelude.negate

c'Decimal'RepRadPer :: RepRadPer -> Decimal
c'Decimal'RepRadPer x = case x of
  Moderate nrc -> Lens.over coefficient (const 0)
    . c'DecZero'NilRadPer $ nrc
  Extreme (Polarized brimRadPer pole) ->
      Lens.over coefficient (changeSign . Pos.c'Integer'Positive)
      . c'DecPositive'BrimRadPer
      $ brimRadPer
      where
        changeSign
          | pole == positive = id
          | otherwise = Prelude.negate

c'Decimal'RepAnyRadix :: RepAnyRadix -> Decimal
c'Decimal'RepAnyRadix = either c'Decimal'RepRadCom c'Decimal'RepRadPer

c'DecPositive'BrimAnyRadix :: BrimAnyRadix -> DecPositive
c'DecPositive'BrimAnyRadix
  = either c'DecPositive'BrimRadCom c'DecPositive'BrimRadPer

c'DecZero'NilAnyRadix :: NilAnyRadix -> DecZero
c'DecZero'NilAnyRadix = either c'DecZero'NilRadCom c'DecZero'NilRadPer

c'Decimal'DecNonZero :: DecNonZero -> Decimal
c'Decimal'DecNonZero = fmap c'Integer'NonZero

c'Decimal'Troiload :: Troika.Troiload -> Decimal
c'Decimal'Troiload x = case x of
  Troika.QC q _ -> c'Decimal'RepAnyRadix q
  Troika.Q q -> c'Decimal'RepAnyRadix q
  Troika.SC qnz -> fmap c'Integer'NonZero qnz
  Troika.S qnz -> fmap c'Integer'NonZero qnz
  Troika.UC rnn s _ ->
    fmap c'Integer'NonZero
    . c'DecNonZero'DecPositive s
    . c'DecPositive'BrimAnyRadix
    $ rnn
  Troika.NC nilAnyRadix _ ->
    fmap (const 0) . c'DecZero'NilAnyRadix $ nilAnyRadix
  Troika.US rnn s ->
    fmap c'Integer'NonZero
    . c'DecNonZero'DecPositive s
    . c'DecPositive'BrimAnyRadix
    $ rnn
  Troika.UU nil -> fmap (const 0) . c'DecZero'NilAnyRadix $ nil
  Troika.C qnz -> fmap c'Integer'NonZero qnz
  Troika.E qnz -> fmap c'Integer'NonZero qnz

pole'Troika :: Troika.Troika -> Maybe Pole
pole'Troika (Troika.Troika _ ei) = case ei of
  Left tl -> pole'Decimal . c'Decimal'Troiload $ tl
  Right dec -> pole'Decimal dec

c'Amount'Troika :: Troika.Troika -> A.Amount
c'Amount'Troika (Troika.Troika cy ei) = A.Amount cy
  (either c'Decimal'Troiload id ei)

-- Trio
oneCommodity :: Imbalance -> Either Trio.TrioError (Commodity.Commodity, DecNonZero)
oneCommodity (Imbalance imb) = case M.toList imb of
  [] -> Left Trio.NoImbalance
  (cy, q):[] -> return (cy, q)
  x:y:xs -> Left $ Trio.MultipleImbalance x y xs

notSameSide :: Pole -> Pole -> Either Trio.TrioError ()
notSameSide x y
  | x == y = Left $ Trio.BalanceIsSameSide x
  | otherwise = return ()

lookupCommodity :: Imbalance -> Commodity.Commodity -> Either Trio.TrioError DecNonZero
lookupCommodity (Imbalance imb) cy = case M.lookup cy imb of
  Nothing -> Left $ Trio.CommodityNotFound cy
  Just dnz -> return dnz

rnnIsSmallerAbsoluteValue
  :: BrimAnyRadix
  -> DecNonZero
  -> Either Trio.TrioError ()
rnnIsSmallerAbsoluteValue qnr qnz
  | _coefficient qnr'' < _coefficient qnz'' = return ()
  | otherwise = Left $ Trio.UnsignedTooLarge qnr qnz
  where
    qnr' = fmap Pos.c'Integer'Positive . c'DecPositive'BrimAnyRadix $ qnr
    qnz' = c'Decimal'DecNonZero qnz
    (qnr'', qnz'') = equalizeExponents qnr' qnz'

trioToAmount :: Imbalance -> Trio.Trio -> Either Trio.TrioError A.Amount

trioToAmount _ (Trio.QC qnr cy _) = Right $ A.Amount cy (c'Decimal'RepAnyRadix qnr)

trioToAmount imb (Trio.Q qnr) = fmap f $ oneCommodity imb
  where
    f (cy, _) = A.Amount cy (c'Decimal'RepAnyRadix qnr)

trioToAmount imb (Trio.SC s cy) = do
  qnz <- lookupCommodity imb cy
  let qtSide = Lens.view (coefficient . NZ.pole) qnz
  notSameSide qtSide s
  return
    . A.Amount cy
    . fmap (Prelude.negate . c'Integer'NonZero)
    $ qnz

trioToAmount imb (Trio.S s) = do
  (cy, qnz) <- oneCommodity imb
  let qtSide = Lens.view (coefficient . NZ.pole) qnz
  notSameSide s qtSide
  return
    . A.Amount cy
    . fmap (Prelude.negate . c'Integer'NonZero)
    $ qnz

trioToAmount imb (Trio.UC qnr cy _) = do
  qnz <- lookupCommodity imb cy
  return
    . A.Amount cy
    . Lens.over coefficient c'Integer'NonZero
    . Lens.set (coefficient . NZ.pole) (Lens.view (coefficient . NZ.pole) qnz)
    . fmap NZ.c'NonZero'Positive
    . c'DecPositive'BrimAnyRadix
    $ qnr

trioToAmount _ (Trio.NC nilAnyRadix cy _) = Right (A.Amount cy qty)
  where
    qty = fmap (const 0) . c'DecZero'NilAnyRadix $ nilAnyRadix

trioToAmount imb (Trio.US qnr) = do
  (cy, qnz) <- oneCommodity imb
  rnnIsSmallerAbsoluteValue qnr qnz
  return
    . A.Amount cy
    . Lens.over coefficient c'Integer'NonZero
    . Lens.set (coefficient . NZ.pole) (Lens.view (coefficient . NZ.pole) qnz)
    . fmap NZ.c'NonZero'Positive
    . c'DecPositive'BrimAnyRadix
    $ qnr

trioToAmount imb (Trio.UU nil) = do
  (cy, _) <- oneCommodity imb
  return (A.Amount cy (fmap (const 0) (c'DecZero'NilAnyRadix nil)))

trioToAmount imb (Trio.C cy) = do
  qnz <- lookupCommodity imb cy
  return
    . A.Amount cy
    . fmap c'Integer'NonZero
    . Lens.set (coefficient . NZ.pole) (Lens.view (coefficient . NZ.pole) qnz)
    $ qnz

trioToAmount imb Trio.E = do
  (cy, qnz) <- oneCommodity imb
  return
    . A.Amount cy
    . fmap c'Integer'NonZero
    . Lens.set (coefficient . NZ.pole) (Lens.view (coefficient . NZ.pole) qnz)
    $ qnz


trioToTroiload
  :: Imbalance
  -> Trio.Trio
  -> Either Trio.TrioError (Troika.Troiload, Commodity.Commodity)

trioToTroiload _ (Trio.QC qnr cy ar) = Right (Troika.QC qnr ar, cy)

trioToTroiload imb (Trio.Q qnr) = fmap f $ oneCommodity imb
  where
    f (cy, _) = (Troika.Q qnr, cy)

trioToTroiload imb (Trio.SC s cy) = do
  qnz <- lookupCommodity imb cy
  let qtSide = Lens.view (coefficient . NZ.pole) qnz
  notSameSide qtSide s
  return (Troika.SC qnz, cy)

trioToTroiload imb (Trio.S s) = do
  (cy, qnz) <- oneCommodity imb
  let qtSide = Lens.view (coefficient . NZ.pole) qnz
  notSameSide s qtSide
  return (Troika.S qnz, cy)

trioToTroiload imb (Trio.UC qnr cy ar) = do
  qnz <- lookupCommodity imb cy
  return (Troika.UC qnr (opposite . Lens.view (coefficient . NZ.pole) $ qnz) ar, cy)

trioToTroiload _ (Trio.NC nilAnyRadix cy ar) = Right (troiload, cy)
  where
    troiload = Troika.NC nilAnyRadix ar

trioToTroiload imb (Trio.US brim) = do
  (cy, qnz) <- oneCommodity imb
  rnnIsSmallerAbsoluteValue brim qnz
  let pole' = Lens.view (coefficient . NZ.pole . Lens.to opposite) qnz
  return (Troika.US brim pole', cy)

trioToTroiload imb (Trio.UU nil) = do
  (cy, _) <- oneCommodity imb
  return (Troika.UU nil, cy)

trioToTroiload imb (Trio.C cy) = do
  qnz <- lookupCommodity imb cy
  let qnz' = Lens.over (coefficient . NZ.pole) opposite qnz
  return (Troika.C qnz', cy)

trioToTroiload imb Trio.E = do
  (cy, qnz) <- oneCommodity imb
  return (Troika.E (Lens.over (coefficient . NZ.pole) opposite qnz), cy)