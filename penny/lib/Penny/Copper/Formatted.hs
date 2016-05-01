{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
-- | Productions that have more than one possible value.  A sensible
-- default value is used.

module Penny.Copper.Formatted where

import Penny.Copper.Char
import Penny.Copper.Optics
import Penny.Copper.Types
import Penny.Copper.Singleton
import Penny.NonNegative (NonNegative)
import qualified Penny.NonNegative as NonNegative
import Penny.Polar
import Penny.Positive (Positive)
import qualified Penny.Positive as Positive

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as X
import qualified Control.Lens as Lens
import Pinchot (NonEmpty(NonEmpty))

-- | Grouper of thin space
fGrouper :: Grouper Char ()
fGrouper = Grouper'ThinSpace sThinSpace

-- | Period grouper
fGrpRadCom :: GrpRadCom Char ()
fGrpRadCom = GrpRadCom'Period sPeriod

-- | Comma grouper
fGrpRadPer :: GrpRadPer Char ()
fGrpRadPer = GrpRadPer'Comma sComma

fDigitGroupRadCom
  :: D0'9 Char ()
  -> Seq (D0'9 Char ())
  -> DigitGroupRadCom Char ()
fDigitGroupRadCom d1 ds
  = DigitGroupRadCom fGrpRadCom d1 (D0'9'Star ds)

fDigitGroupRadPer
  :: D0'9 Char ()
  -> Seq (D0'9 Char ())
  -> DigitGroupRadPer Char ()
fDigitGroupRadPer d1 ds
  = DigitGroupRadPer fGrpRadPer d1 (D0'9'Star ds)

-- | Returns a number of zeroes the same as the 'NonNegative'.
fZeroes :: NonNegative -> Zero'Star Char ()
fZeroes = Zero'Star . go Seq.empty
  where
    go acc nn = case NonNegative.prev nn of
      Nothing -> acc
      Just p -> go (Lens.cons sZero acc) p

fRadixZeroesRadCom :: NonNegative -> RadixZeroesRadCom Char ()
fRadixZeroesRadCom nn = RadixZeroesRadCom sRadixCom (fZeroes nn)

fRadixZeroesRadPer :: NonNegative -> RadixZeroesRadPer Char ()
fRadixZeroesRadPer nn = RadixZeroesRadPer sRadixPer (fZeroes nn)

fZeroGroupRadCom :: Positive -> ZeroGroupRadCom Char ()
fZeroGroupRadCom pos
  = ZeroGroupRadCom fGrpRadCom sZero (fZeroes rest)
  where
    rest = case Positive.prev pos of
      Nothing -> NonNegative.zero
      Just p -> NonNegative.c'NonNegative'Positive p

fZeroGroupRadPer :: Positive -> ZeroGroupRadPer Char ()
fZeroGroupRadPer pos
  = ZeroGroupRadPer fGrpRadPer sZero (fZeroes rest)
  where
    rest = case Positive.prev pos of
      Nothing -> NonNegative.zero
      Just p -> NonNegative.c'NonNegative'Positive p

-- | Has a leading zero and period for grouper.
fNilGroupedRadCom
  :: Positive
  -- ^ Number of leading zeroes before first grouping character
  -> ZeroGroupRadCom Char ()
  -> Seq (ZeroGroupRadCom Char ())
  -> NilGroupedRadCom Char ()
fNilGroupedRadCom lead g1 gs = NilGroupedRadCom mz0 r1 z2 z3 zg4
  where
    mz0 = Zero'Opt (Just sZero)
    r1 = sRadixCom
    z2 = sZero
    z3 = case Positive.prev lead of
      Nothing -> Zero'Star Seq.empty
      Just p -> fZeroes (NonNegative.c'NonNegative'Positive p)
    zg4 = ZeroGroupRadCom'Plus $ NonEmpty g1 gs

-- | Has a leading zero and comma for grouper.
fNilGroupedRadPer
  :: Positive
  -- ^ Number of leading zeroes before first grouping character
  -> ZeroGroupRadPer Char ()
  -> Seq (ZeroGroupRadPer Char ())
  -> NilGroupedRadPer Char ()
fNilGroupedRadPer lead g1 gs = NilGroupedRadPer mz0 r1 z2 z3 zg4
  where
    mz0 = Zero'Opt (Just sZero)
    r1 = sRadixPer
    z2 = sZero
    z3 = case Positive.prev lead of
      Nothing -> Zero'Star Seq.empty
      Just p -> fZeroes (NonNegative.c'NonNegative'Positive p)
    zg4 = ZeroGroupRadPer'Plus $ NonEmpty g1 gs

-- | Has a leading zero.
fNilUngroupedRadCom
  :: NonNegative
  -- ^ Number of zeroes after the decimal point
  -> NilUngroupedRadCom Char ()
fNilUngroupedRadCom nn = case NonNegative.c'Positive'NonNegative nn of
  Nothing -> NUZeroRadCom sZero (RadixZeroesRadCom'Opt Nothing)
  Just _ -> NUZeroRadCom sZero
    (RadixZeroesRadCom'Opt (Just (fRadixZeroesRadCom nn)))

-- | Has a leading zero.
fNilUngroupedRadPer
  :: NonNegative
  -- ^ Number of zeroes after the decimal point
  -> NilUngroupedRadPer Char ()
fNilUngroupedRadPer nn = case NonNegative.c'Positive'NonNegative nn of
  Nothing -> NUZeroRadPer sZero (RadixZeroesRadPer'Opt Nothing)
  Just _ -> NUZeroRadPer sZero
    (RadixZeroesRadPer'Opt (Just (fRadixZeroesRadPer nn)))

fRadixComDigits :: Seq (D0'9 Char ()) -> RadixComDigits Char ()
fRadixComDigits ds = RadixComDigits sRadixCom (D0'9'Star ds)

fRadixPerDigits :: Seq (D0'9 Char ()) -> RadixPerDigits Char ()
fRadixPerDigits ds = RadixPerDigits sRadixPer (D0'9'Star ds)

fBUGreaterThanOneRadCom
  :: D1'9 Char ()
  -- ^ First digit, to left of radix
  -> Seq (D0'9 Char ())
  -- ^ Remaining digits, to left of radix
  -> Seq (D0'9 Char ())
  -- ^ Remaining digits, to right of radix
  -> BrimUngroupedRadCom Char ()
fBUGreaterThanOneRadCom d1 ds rs = BUGreaterThanOneRadCom d1 (D0'9'Star ds)
  (RadixComDigits'Opt may)
  where
    may
      | Seq.null rs = Nothing
      | otherwise = Just (fRadixComDigits rs)

fBUGreaterThanOneRadPer
  :: D1'9 Char ()
  -- ^ First digit, to left of radix
  -> Seq (D0'9 Char ())
  -- ^ Remaining digits, to left of radix
  -> Seq (D0'9 Char ())
  -- ^ Remaining digits, to right of radix
  -> BrimUngroupedRadPer Char ()
fBUGreaterThanOneRadPer d1 ds rs = BUGreaterThanOneRadPer d1 (D0'9'Star ds)
  (RadixPerDigits'Opt may)
  where
    may
      | Seq.null rs = Nothing
      | otherwise = Just (fRadixPerDigits rs)

fBULessThanOneRadCom
  :: NonNegative
  -- ^ Number of zeroes to right of radix
  -> D1'9 Char ()
  -- ^ First non-zero digit to right of radix
  -> Seq (D0'9 Char ())
  -- ^ Remaining digits to right of radix
  -> BrimUngroupedRadCom Char ()
fBULessThanOneRadCom zs d1 ds = BULessThanOneRadCom
  (Zero'Opt (Just sZero)) sRadixCom (fZeroes zs) d1 (D0'9'Star ds)

fBULessThanOneRadPer
  :: NonNegative
  -- ^ Number of zeroes to right of radix
  -> D1'9 Char ()
  -- ^ First non-zero digit to right of radix
  -> Seq (D0'9 Char ())
  -- ^ Remaining digits to right of radix
  -> BrimUngroupedRadPer Char ()
fBULessThanOneRadPer zs d1 ds = BULessThanOneRadPer
  (Zero'Opt (Just sZero)) sRadixPer (fZeroes zs) d1 (D0'9'Star ds)

-- # BrimGrouped, comma radix

fBG8NovemRadCom
  :: D1'9 Char ()
  -> Seq (D0'9 Char ())
  -> Seq (D0'9 Char (), Seq (D0'9 Char ()))
  -> BG8RadCom Char ()
fBG8NovemRadCom d1 ds gs = BG8NovemRadCom d1 (D0'9'Star ds) gs'
  where
    gs' = DigitGroupRadCom'Star (fmap (uncurry fDigitGroupRadCom) gs)

fBG8GroupRadCom
  :: BG7RadCom Char ()
  -> BG8RadCom Char ()
fBG8GroupRadCom b7 = BG8GroupRadCom fGrpRadCom b7

fBG7ZeroesRadCom
  :: Positive
  -> BG8RadCom Char ()
  -> BG7RadCom Char ()
fBG7ZeroesRadCom pos b8 = BG7ZeroesRadCom sZero zs b8
  where
    zs = case Positive.prev pos of
      Nothing -> Zero'Star Seq.empty
      Just nn -> fZeroes (NonNegative.c'NonNegative'Positive nn)

fBG7NovemRadCom
  :: D1'9 Char ()
  -> Seq (D0'9 Char ())
  -> Seq (D0'9 Char (), Seq ((D0'9) Char ()))
  -> BG7RadCom Char ()
fBG7NovemRadCom d1 ds gs = BG7NovemRadCom d1 (D0'9'Star ds) gs'
  where
    gs' = DigitGroupRadCom'Star (fmap (uncurry fDigitGroupRadCom) gs)

fBG6NovemRadCom
  :: D1'9 Char ()
  -> Seq (D0'9 Char ())
  -> D0'9 Char ()
  -> Seq (D0'9 Char ())
  -> Seq (D0'9 Char (), Seq ((D0'9) Char ()))
  -> BG6RadCom Char ()
fBG6NovemRadCom d1 ds2 d3 ds4 gs5 = BG6NovemRadCom d1 (D0'9'Star ds2)
  fGrpRadCom d3 (D0'9'Star ds4)
  (DigitGroupRadCom'Star (fmap (uncurry fDigitGroupRadCom) gs5))

fBG6GroupRadCom
  :: BG7RadCom Char ()
  -> BG6RadCom Char ()
fBG6GroupRadCom b7 = BG6GroupRadCom fGrpRadCom b7

fBG5NovemRadCom
  :: D1'9 Char ()
  -> Seq (D0'9 Char ())
  -> D0'9 Char ()
  -> Seq (D0'9 Char ())
  -> Seq (D0'9 Char (), Seq (D0'9 Char ()))
  -> BG5RadCom Char ()
fBG5NovemRadCom d0 ds1 d2 ds3 gs4
  = BG5NovemRadCom d0 (D0'9'Star ds1) fGrpRadCom d2 (D0'9'Star ds3)
    (DigitGroupRadCom'Star (fmap (uncurry fDigitGroupRadCom) gs4))

fBG5ZeroRadCom
  :: Positive
  -- ^ Number of leading zeroes
  -> BG6RadCom Char ()
  -> BG5RadCom Char ()
fBG5ZeroRadCom pos b6 = BG5ZeroRadCom sZero rest b6
  where
    rest = case Positive.prev pos of
      Nothing -> fZeroes NonNegative.zero
      Just pos' -> fZeroes (NonNegative.c'NonNegative'Positive pos')

fBG4DigitRadCom
  :: D0'9 Char ()
  -> Seq (D0'9 Char ())
  -> Seq (D0'9 Char (), Seq (D0'9 Char ()))
  -> BG4RadCom Char ()
fBG4DigitRadCom d1 ds2 dss
  = BG4DigitRadCom d1 (D0'9'Star ds2)
    (DigitGroupRadCom'Star (fmap (uncurry fDigitGroupRadCom) dss))

fBG4NilRadCom :: BG4RadCom Char ()
fBG4NilRadCom = BG4NilRadCom

fBG3RadixRadCom :: BG4RadCom Char () -> BG3RadCom Char ()
fBG3RadixRadCom b4 = BG3RadixRadCom sRadixCom b4

fBG3NilRadCom :: BG3RadCom Char ()
fBG3NilRadCom = BG3NilRadCom

fBG1GroupOnLeftRadCom
  :: D0'9 Char ()
  -> Seq (D0'9 Char ())
  -> Seq (D0'9 Char (), Seq (D0'9 Char ()))
  -> BG3RadCom Char ()
  -> BG1RadCom Char ()
fBG1GroupOnLeftRadCom d0 ds1 gs2 b3 = BG1GroupOnLeftRadCom fGrpRadCom d0
  (D0'9'Star ds1)
  (DigitGroupRadCom'Star (fmap (uncurry fDigitGroupRadCom) gs2))
  b3

fBG1GroupOnRightRadCom
  :: D0'9 Char ()
  -> Seq (D0'9 Char ())
  -> D0'9 Char ()
  -> Seq (D0'9 Char ())
  -> Seq (D0'9 Char (), Seq (D0'9 Char ()))
  -> BG1RadCom Char ()
fBG1GroupOnRightRadCom d1 ds2 d4 ds5 g6
  = BG1GroupOnRightRadCom sRadixCom d1 (D0'9'Star ds2) fGrpRadCom
  d4 (D0'9'Star ds5)
  (DigitGroupRadCom'Star (fmap (uncurry fDigitGroupRadCom) g6))

fBGGreaterThanOneRadCom
  :: D1'9 Char ()
  -> Seq (D0'9 Char ())
  -> BG1RadCom Char ()
  -> BrimGroupedRadCom Char ()
fBGGreaterThanOneRadCom d0 ds1 b1
  = BGGreaterThanOneRadCom d0 (D0'9'Star ds1) b1

fBGLessThanOneRadCom
  :: BG5RadCom Char ()
  -> BrimGroupedRadCom Char ()
fBGLessThanOneRadCom b5
  = BGLessThanOneRadCom (Zero'Opt (Just sZero)) sRadixCom b5

-- # BrimGrouped, period radix

fBG8NovemRadPer
  :: D1'9 Char ()
  -> Seq (D0'9 Char ())
  -> Seq (D0'9 Char (), Seq (D0'9 Char ()))
  -> BG8RadPer Char ()
fBG8NovemRadPer d1 ds gs = BG8NovemRadPer d1 (D0'9'Star ds) gs'
  where
    gs' = DigitGroupRadPer'Star (fmap (uncurry fDigitGroupRadPer) gs)

fBG8GroupRadPer
  :: BG7RadPer Char ()
  -> BG8RadPer Char ()
fBG8GroupRadPer b7 = BG8GroupRadPer fGrpRadPer b7

fBG7ZeroesRadPer
  :: Positive
  -> BG8RadPer Char ()
  -> BG7RadPer Char ()
fBG7ZeroesRadPer pos b8 = BG7ZeroesRadPer sZero zs b8
  where
    zs = case Positive.prev pos of
      Nothing -> Zero'Star Seq.empty
      Just nn -> fZeroes (NonNegative.c'NonNegative'Positive nn)

fBG7NovemRadPer
  :: D1'9 Char ()
  -> Seq (D0'9 Char ())
  -> Seq (D0'9 Char (), Seq (D0'9 Char ()))
  -> BG7RadPer Char ()
fBG7NovemRadPer d1 ds gs = BG7NovemRadPer d1 (D0'9'Star ds) gs'
  where
    gs' = DigitGroupRadPer'Star (fmap (uncurry fDigitGroupRadPer) gs)

fBG6NovemRadPer
  :: D1'9 Char ()
  -> Seq (D0'9 Char ())
  -> D0'9 Char ()
  -> Seq (D0'9 Char ())
  -> Seq (D0'9 Char (), Seq (D0'9 Char ()))
  -> BG6RadPer Char ()
fBG6NovemRadPer d1 ds2 d3 ds4 gs5 = BG6NovemRadPer d1 (D0'9'Star ds2)
  fGrpRadPer d3 (D0'9'Star ds4)
  (DigitGroupRadPer'Star (fmap (uncurry fDigitGroupRadPer) gs5))

fBG6GroupRadPer
  :: BG7RadPer Char ()
  -> BG6RadPer Char ()
fBG6GroupRadPer b7 = BG6GroupRadPer fGrpRadPer b7

fBG5NovemRadPer
  :: D1'9 Char ()
  -> Seq (D0'9 Char ())
  -> D0'9 Char ()
  -> Seq (D0'9 Char ())
  -> Seq (D0'9 Char (), Seq (D0'9 Char ()))
  -> BG5RadPer Char ()
fBG5NovemRadPer d0 ds1 d2 ds3 gs4
  = BG5NovemRadPer d0 (D0'9'Star ds1) fGrpRadPer d2 (D0'9'Star ds3)
    (DigitGroupRadPer'Star (fmap (uncurry fDigitGroupRadPer) gs4))

fBG5ZeroRadPer
  :: Positive
  -- ^ Number of leading zeroes
  -> BG6RadPer Char ()
  -> BG5RadPer Char ()
fBG5ZeroRadPer pos b6 = BG5ZeroRadPer sZero rest b6
  where
    rest = case Positive.prev pos of
      Nothing -> fZeroes NonNegative.zero
      Just pos' -> fZeroes (NonNegative.c'NonNegative'Positive pos')

fBG4DigitRadPer
  :: D0'9 Char ()
  -> Seq (D0'9 Char ())
  -> Seq (D0'9 Char (), Seq (D0'9 Char ()))
  -> BG4RadPer Char ()
fBG4DigitRadPer d1 ds2 dss
  = BG4DigitRadPer d1 (D0'9'Star ds2)
    (DigitGroupRadPer'Star (fmap (uncurry fDigitGroupRadPer) dss))

fBG4NilRadPer :: BG4RadPer Char ()
fBG4NilRadPer = BG4NilRadPer

fBG3RadixRadPer :: BG4RadPer Char () -> BG3RadPer Char ()
fBG3RadixRadPer b4 = BG3RadixRadPer sRadixPer b4

fBG3NilRadPer :: BG3RadPer Char ()
fBG3NilRadPer = BG3NilRadPer

fBG1GroupOnLeftRadPer
  :: D0'9 Char ()
  -> Seq (D0'9 Char ())
  -> Seq (D0'9 Char (), Seq (D0'9 Char ()))
  -> BG3RadPer Char ()
  -> BG1RadPer Char ()
fBG1GroupOnLeftRadPer d0 ds1 gs2 b3 = BG1GroupOnLeftRadPer fGrpRadPer d0
  (D0'9'Star ds1)
  (DigitGroupRadPer'Star (fmap (uncurry fDigitGroupRadPer) gs2))
  b3

fBG1GroupOnRightRadPer
  :: D0'9 Char ()
  -> Seq (D0'9 Char ())
  -> D0'9 Char ()
  -> Seq (D0'9 Char ())
  -> Seq (D0'9 Char (), Seq (D0'9 Char ()))
  -> BG1RadPer Char ()
fBG1GroupOnRightRadPer d1 ds2 d4 ds5 g6
  = BG1GroupOnRightRadPer sRadixPer d1 (D0'9'Star ds2) fGrpRadPer
  d4 (D0'9'Star ds5)
  (DigitGroupRadPer'Star (fmap (uncurry fDigitGroupRadPer) g6))

fBGGreaterThanOneRadPer
  :: D1'9 Char ()
  -> Seq (D0'9 Char ())
  -> BG1RadPer Char ()
  -> BrimGroupedRadPer Char ()
fBGGreaterThanOneRadPer d0 ds1 b1
  = BGGreaterThanOneRadPer d0 (D0'9'Star ds1) b1

fBGLessThanOneRadPer
  :: BG5RadPer Char ()
  -> BrimGroupedRadPer Char ()
fBGLessThanOneRadPer b5
  = BGLessThanOneRadPer (Zero'Opt (Just sZero)) sRadixPer b5

-- # Dates. Use a hyphen as separator.

fDateSep :: DateSep Char ()
fDateSep = DateSep'Hyphen sHyphen

fJan :: Days31 Char () -> MonthDay Char ()
fJan d = Jan sZero sOne fDateSep d

fFeb :: Days28 Char () -> MonthDay Char ()
fFeb d = Feb sZero sTwo fDateSep d

fMar :: Days31 Char () -> MonthDay Char ()
fMar d = Mar sZero sThree fDateSep d

fApr :: Days30 Char () -> MonthDay Char ()
fApr d = Apr sZero sFour fDateSep d

fMay :: Days31 Char () -> MonthDay Char ()
fMay d = May sZero sFive fDateSep d

fJun :: Days30 Char () -> MonthDay Char ()
fJun d = Jun sZero sSix fDateSep d

fJul :: Days31 Char () -> MonthDay Char ()
fJul d = Jul sZero sSeven fDateSep d

fAug :: Days31 Char () -> MonthDay Char ()
fAug d = Aug sZero sEight fDateSep d

fSep :: Days30 Char () -> MonthDay Char ()
fSep d = Sep sZero sNine fDateSep d

fOct :: Days31 Char () -> MonthDay Char ()
fOct d = Oct sOne sZero fDateSep d

fNov :: Days30 Char () -> MonthDay Char ()
fNov d = Nov sOne sOne fDateSep d

fDec :: Days31 Char () -> MonthDay Char ()
fDec d = Dec sOne sTwo fDateSep d

fNonLeapDay
  :: Year Char ()
  -> MonthDay Char ()
  -> NonLeapDay Char ()
fNonLeapDay y md = NonLeapDay y fDateSep md

fLeapDay :: LeapYear Char () -> LeapDay Char ()
fLeapDay ly = LeapDay ly fDateSep sZero sTwo fDateSep sTwo sNine

fComment :: Seq Char -> Maybe (Comment Char ())
fComment = fmap f . sequence . fmap (Lens.preview _CommentChar . makePair)
  where
    f cs = Comment sHash (CommentChar'Star cs) sNewline
    makePair c = (c, ())

fTime
  :: Hours Char ()
  -> (D0'5 Char (), D0'9 Char ())
  -- ^ Minutes
  -> Maybe (D0'5 Char (), D0'9 Char ())
  -- ^ Seconds
  -> Time Char ()
fTime h (m1, m2) s = Time h sColon (Minutes (N0'59 m1 m2)) s'
  where
    s' = case s of
      Nothing -> ColonSeconds'Opt Nothing
      Just (s1, s2) -> ColonSeconds'Opt (Just (ColonSeconds sColon
        (Seconds (N0'59 s1 s2))))

fZone
  :: Pole
  -- ^ Positive or negative
  -> D0'2 Char ()
  -> D0'3 Char ()
  -> D0'9 Char ()
  -> D0'9 Char ()
  -> Zone Char ()
fZone p d0 d1 d2 d3 = Zone sBacktick
  (ZoneHrsMins pm d0 d1 d2 d3)
  where
    pm | p == positive = PluMin'Plus sPlus
       | otherwise = PluMin'Minus sMinus

space :: White'Star Char ()
space = White'Star . Seq.singleton
  $ White'Space sSpace

newline :: White'Star Char ()
newline = White'Star . Seq.singleton . White'Newline $ sNewline

noSpace :: White'Star Char ()
noSpace = White'Star Seq.empty

fNextTree :: Tree Char () -> NextTree Char ()
fNextTree tree = NextTree noSpace sComma space tree

fNextTree'Star :: Seq (Tree Char ()) -> NextTree'Star Char ()
fNextTree'Star = NextTree'Star . fmap fNextTree

fForest :: Tree Char () -> Seq (Tree Char ()) -> Forest Char ()
fForest t1 ts = Forest t1 (fNextTree'Star ts)

fBracketedForest :: Forest Char () -> BracketedForest Char ()
fBracketedForest f
  = BracketedForest sOpenSquare space f space sCloseSquare

fTree :: Scalar Char () -> Maybe (Forest Char ()) -> Tree Char ()
fTree sc mayForest = Tree'ScalarMaybeForest
  (ScalarMaybeForest sc (WhitesBracketedForest'Opt may))
  where
    may = case mayForest of
      Nothing -> Nothing
      Just fr -> Just (WhitesBracketedForest space (fBracketedForest fr))

spinster :: Scalar Char () -> Tree Char ()
spinster s = fTree s Nothing

orphans :: Tree Char () -> Seq (Tree Char ()) -> Tree Char ()
orphans t1 ts = Tree'ForestMaybeScalar
  (ForestMaybeScalar (fBracketedForest (fForest t1 ts))
                     (WhitesScalar'Opt Nothing))

-- | Makes an unquoted scalar if possible; otherwise, makes a quoted
-- scalar.
textScalar :: Text -> Scalar Char ()
textScalar txt = case fString . Seq.fromList . X.unpack $ txt of
  Left us -> Scalar'UnquotedString us
  Right qs -> Scalar'QuotedString qs

fDateTimeZone
  :: Date Char ()
  -> Time Char ()
  -> Zone Char ()
  -> Forest Char ()
fDateTimeZone date time zone = fForest dateTree [timeTree, zoneTree]
  where
    dateTree = spinster (Scalar'Date date)
    timeTree = spinster (Scalar'Time time)
    zoneTree = spinster (Scalar'Zone zone)
