{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
-- | Productions that have more than one possible value.  A sensible
-- default value is used.

module Penny.Copper.Formatted where

import Penny.Copper.Types
import Penny.Copper.Singleton
import Penny.NonNegative (NonNegative)
import qualified Penny.NonNegative as NonNegative
import Penny.Polar
import Penny.Positive (Positive)
import qualified Penny.Positive as Positive

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Control.Lens as Lens

-- | Grouper of thin space
fGrouper :: Grouper
fGrouper = Grouper'ThinSpace sThinSpace

-- | Period grouper
fGrpRadCom :: GrpRadCom
fGrpRadCom = GrpRadCom'Period sPeriod

-- | Comma grouper
fGrpRadPer :: GrpRadPer
fGrpRadPer = GrpRadPer'Comma sComma

fDigitGroupRadCom :: D0'9 -> Seq D0'9 -> DigitGroupRadCom
fDigitGroupRadCom d1 ds
  = DigitGroupRadCom fGrpRadCom d1 (D0'9'Seq ds)

fDigitGroupRadPer :: D0'9 -> Seq D0'9 -> DigitGroupRadPer
fDigitGroupRadPer d1 ds
  = DigitGroupRadPer fGrpRadPer d1 (D0'9'Seq ds)

-- | Returns a number of zeroes the same as the 'NonNegative'.
fZeroes :: NonNegative -> Zero'Seq
fZeroes = Zero'Seq . go Seq.empty
  where
    go acc nn = case NonNegative.prev nn of
      Nothing -> acc
      Just p -> go (Lens.cons sZero acc) p

fRadixZeroesRadCom :: NonNegative -> RadixZeroesRadCom
fRadixZeroesRadCom nn = RadixZeroesRadCom sRadixCom (fZeroes nn)

fRadixZeroesRadPer :: NonNegative -> RadixZeroesRadPer
fRadixZeroesRadPer nn = RadixZeroesRadPer sRadixPer (fZeroes nn)

fZeroGroupRadCom :: Positive -> ZeroGroupRadCom
fZeroGroupRadCom pos
  = ZeroGroupRadCom fGrpRadCom sZero (fZeroes rest)
  where
    rest = case Positive.prev pos of
      Nothing -> NonNegative.zero
      Just p -> NonNegative.c'NonNegative'Positive p

fZeroGroupRadPer :: Positive -> ZeroGroupRadPer
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
  -> ZeroGroupRadCom
  -> Seq ZeroGroupRadCom
  -> NilGroupedRadCom
fNilGroupedRadCom lead g1 gs = NilGroupedRadCom mz0 r1 z2 z3 zg4
  where
    mz0 = Zero'Maybe (Just sZero)
    r1 = sRadixCom
    z2 = sZero
    z3 = case Positive.prev lead of
      Nothing -> Zero'Seq Seq.empty
      Just p -> fZeroes (NonNegative.c'NonNegative'Positive p)
    zg4 = ZeroGroupRadCom'Seq1 (g1, gs)

-- | Has a leading zero and comma for grouper.
fNilGroupedRadPer
  :: Positive
  -- ^ Number of leading zeroes before first grouping character
  -> ZeroGroupRadPer
  -> Seq ZeroGroupRadPer
  -> NilGroupedRadPer
fNilGroupedRadPer lead g1 gs = NilGroupedRadPer mz0 r1 z2 z3 zg4
  where
    mz0 = Zero'Maybe (Just sZero)
    r1 = sRadixPer
    z2 = sZero
    z3 = case Positive.prev lead of
      Nothing -> Zero'Seq Seq.empty
      Just p -> fZeroes (NonNegative.c'NonNegative'Positive p)
    zg4 = ZeroGroupRadPer'Seq1 (g1, gs)

-- | Has a leading zero.
fNilUngroupedRadCom
  :: NonNegative
  -- ^ Number of zeroes after the decimal point
  -> NilUngroupedRadCom
fNilUngroupedRadCom nn = case NonNegative.c'Positive'NonNegative nn of
  Nothing -> NUZeroRadCom sZero (RadixZeroesRadCom'Maybe Nothing)
  Just _ -> NUZeroRadCom sZero
    (RadixZeroesRadCom'Maybe (Just (fRadixZeroesRadCom nn)))

-- | Has a leading zero.
fNilUngroupedRadPer
  :: NonNegative
  -- ^ Number of zeroes after the decimal point
  -> NilUngroupedRadPer
fNilUngroupedRadPer nn = case NonNegative.c'Positive'NonNegative nn of
  Nothing -> NUZeroRadPer sZero (RadixZeroesRadPer'Maybe Nothing)
  Just _ -> NUZeroRadPer sZero
    (RadixZeroesRadPer'Maybe (Just (fRadixZeroesRadPer nn)))

fRadixComDigits :: Seq D0'9 -> RadixComDigits
fRadixComDigits ds = RadixComDigits sRadixCom (D0'9'Seq ds)

fRadixPerDigits :: Seq D0'9 -> RadixPerDigits
fRadixPerDigits ds = RadixPerDigits sRadixPer (D0'9'Seq ds)

fBUGreaterThanOneRadCom
  :: D1'9
  -- ^ First digit, to left of radix
  -> Seq D0'9
  -- ^ Remaining digits, to left of radix
  -> Seq D0'9
  -- ^ Remaining digits, to right of radix
  -> BrimUngroupedRadCom
fBUGreaterThanOneRadCom d1 ds rs = BUGreaterThanOneRadCom d1 (D0'9'Seq ds)
  (RadixComDigits'Maybe may)
  where
    may
      | Seq.null rs = Nothing
      | otherwise = Just (fRadixComDigits rs)

fBUGreaterThanOneRadPer
  :: D1'9
  -- ^ First digit, to left of radix
  -> Seq D0'9
  -- ^ Remaining digits, to left of radix
  -> Seq D0'9
  -- ^ Remaining digits, to right of radix
  -> BrimUngroupedRadPer
fBUGreaterThanOneRadPer d1 ds rs = BUGreaterThanOneRadPer d1 (D0'9'Seq ds)
  (RadixPerDigits'Maybe may)
  where
    may
      | Seq.null rs = Nothing
      | otherwise = Just (fRadixPerDigits rs)

fBULessThanOneRadCom
  :: NonNegative
  -- ^ Number of zeroes to right of radix
  -> D1'9
  -- ^ First non-zero digit to right of radix
  -> Seq D0'9
  -- ^ Remaining digits to right of radix
  -> BrimUngroupedRadCom
fBULessThanOneRadCom zs d1 ds = BULessThanOneRadCom
  (Zero'Maybe (Just sZero)) sRadixCom (fZeroes zs) d1 (D0'9'Seq ds)

fBULessThanOneRadPer
  :: NonNegative
  -- ^ Number of zeroes to right of radix
  -> D1'9
  -- ^ First non-zero digit to right of radix
  -> Seq D0'9
  -- ^ Remaining digits to right of radix
  -> BrimUngroupedRadPer
fBULessThanOneRadPer zs d1 ds = BULessThanOneRadPer
  (Zero'Maybe (Just sZero)) sRadixPer (fZeroes zs) d1 (D0'9'Seq ds)

-- # BrimGrouped, comma radix

fBG8NovemRadCom
  :: D1'9
  -> Seq D0'9
  -> Seq (D0'9, Seq D0'9)
  -> BG8RadCom
fBG8NovemRadCom d1 ds gs = BG8NovemRadCom d1 (D0'9'Seq ds) gs'
  where
    gs' = DigitGroupRadCom'Seq (fmap (uncurry fDigitGroupRadCom) gs)

fBG8GroupRadCom
  :: BG7RadCom
  -> BG8RadCom
fBG8GroupRadCom b7 = BG8GroupRadCom fGrpRadCom b7

fBG7ZeroesRadCom
  :: Positive
  -> BG8RadCom
  -> BG7RadCom
fBG7ZeroesRadCom pos b8 = BG7ZeroesRadCom sZero zs b8
  where
    zs = case Positive.prev pos of
      Nothing -> Zero'Seq Seq.empty
      Just nn -> fZeroes (NonNegative.c'NonNegative'Positive nn)

fBG7NovemRadCom
  :: D1'9
  -> Seq D0'9
  -> Seq (D0'9, Seq D0'9)
  -> BG7RadCom
fBG7NovemRadCom d1 ds gs = BG7NovemRadCom d1 (D0'9'Seq ds) gs'
  where
    gs' = DigitGroupRadCom'Seq (fmap (uncurry fDigitGroupRadCom) gs)

fBG6NovemRadCom
  :: D1'9
  -> Seq D0'9
  -> D0'9
  -> Seq D0'9
  -> Seq (D0'9, Seq D0'9)
  -> BG6RadCom
fBG6NovemRadCom d1 ds2 d3 ds4 gs5 = BG6NovemRadCom d1 (D0'9'Seq ds2)
  fGrpRadCom d3 (D0'9'Seq ds4)
  (DigitGroupRadCom'Seq (fmap (uncurry fDigitGroupRadCom) gs5))

fBG6GroupRadCom
  :: BG7RadCom
  -> BG6RadCom
fBG6GroupRadCom b7 = BG6GroupRadCom fGrpRadCom b7

fBG5NovemRadCom
  :: D1'9
  -> Seq D0'9
  -> D0'9
  -> Seq D0'9
  -> Seq (D0'9, Seq D0'9)
  -> BG5RadCom
fBG5NovemRadCom d0 ds1 d2 ds3 gs4
  = BG5NovemRadCom d0 (D0'9'Seq ds1) fGrpRadCom d2 (D0'9'Seq ds3)
    (DigitGroupRadCom'Seq (fmap (uncurry fDigitGroupRadCom) gs4))

fBG5ZeroRadCom
  :: Positive
  -- ^ Number of leading zeroes
  -> BG6RadCom
  -> BG5RadCom
fBG5ZeroRadCom pos b6 = BG5ZeroRadCom sZero rest b6
  where
    rest = case Positive.prev pos of
      Nothing -> fZeroes NonNegative.zero
      Just pos' -> fZeroes (NonNegative.c'NonNegative'Positive pos')

fBG4DigitRadCom
  :: D0'9
  -> Seq D0'9
  -> Seq (D0'9, Seq D0'9)
  -> BG4RadCom
fBG4DigitRadCom d1 ds2 dss
  = BG4DigitRadCom d1 (D0'9'Seq ds2)
    (DigitGroupRadCom'Seq (fmap (uncurry fDigitGroupRadCom) dss))

fBG4NilRadCom :: BG4RadCom
fBG4NilRadCom = BG4NilRadCom

fBG3RadixRadCom :: BG4RadCom -> BG3RadCom
fBG3RadixRadCom b4 = BG3RadixRadCom sRadixCom b4

fBG3NilRadCom :: BG3RadCom
fBG3NilRadCom = BG3NilRadCom

fBG1GroupOnLeftRadCom
  :: D0'9
  -> Seq D0'9
  -> Seq (D0'9, Seq D0'9)
  -> BG3RadCom
  -> BG1RadCom
fBG1GroupOnLeftRadCom d0 ds1 gs2 b3 = BG1GroupOnLeftRadCom fGrpRadCom d0
  (D0'9'Seq ds1)
  (DigitGroupRadCom'Seq (fmap (uncurry fDigitGroupRadCom) gs2))
  b3

fBG1GroupOnRightRadCom
  :: D0'9
  -> Seq D0'9
  -> D0'9
  -> Seq D0'9
  -> Seq (D0'9, Seq D0'9)
  -> BG1RadCom
fBG1GroupOnRightRadCom d1 ds2 d4 ds5 g6
  = BG1GroupOnRightRadCom sRadixCom d1 (D0'9'Seq ds2) fGrpRadCom
  d4 (D0'9'Seq ds5)
  (DigitGroupRadCom'Seq (fmap (uncurry fDigitGroupRadCom) g6))

fBGGreaterThanOneRadCom
  :: D1'9
  -> Seq D0'9
  -> BG1RadCom
  -> BrimGroupedRadCom
fBGGreaterThanOneRadCom d0 ds1 b1
  = BGGreaterThanOneRadCom d0 (D0'9'Seq ds1) b1

fBGLessThanOneRadCom
  :: BG5RadCom
  -> BrimGroupedRadCom
fBGLessThanOneRadCom b5
  = BGLessThanOneRadCom (Zero'Maybe (Just sZero)) sRadixCom b5

-- # BrimGrouped, period radix

fBG8NovemRadPer
  :: D1'9
  -> Seq D0'9
  -> Seq (D0'9, Seq D0'9)
  -> BG8RadPer
fBG8NovemRadPer d1 ds gs = BG8NovemRadPer d1 (D0'9'Seq ds) gs'
  where
    gs' = DigitGroupRadPer'Seq (fmap (uncurry fDigitGroupRadPer) gs)

fBG8GroupRadPer
  :: BG7RadPer
  -> BG8RadPer
fBG8GroupRadPer b7 = BG8GroupRadPer fGrpRadPer b7

fBG7ZeroesRadPer
  :: Positive
  -> BG8RadPer
  -> BG7RadPer
fBG7ZeroesRadPer pos b8 = BG7ZeroesRadPer sZero zs b8
  where
    zs = case Positive.prev pos of
      Nothing -> Zero'Seq Seq.empty
      Just nn -> fZeroes (NonNegative.c'NonNegative'Positive nn)

fBG7NovemRadPer
  :: D1'9
  -> Seq D0'9
  -> Seq (D0'9, Seq D0'9)
  -> BG7RadPer
fBG7NovemRadPer d1 ds gs = BG7NovemRadPer d1 (D0'9'Seq ds) gs'
  where
    gs' = DigitGroupRadPer'Seq (fmap (uncurry fDigitGroupRadPer) gs)

fBG6NovemRadPer
  :: D1'9
  -> Seq D0'9
  -> D0'9
  -> Seq D0'9
  -> Seq (D0'9, Seq D0'9)
  -> BG6RadPer
fBG6NovemRadPer d1 ds2 d3 ds4 gs5 = BG6NovemRadPer d1 (D0'9'Seq ds2)
  fGrpRadPer d3 (D0'9'Seq ds4)
  (DigitGroupRadPer'Seq (fmap (uncurry fDigitGroupRadPer) gs5))

fBG6GroupRadPer
  :: BG7RadPer
  -> BG6RadPer
fBG6GroupRadPer b7 = BG6GroupRadPer fGrpRadPer b7

fBG5NovemRadPer
  :: D1'9
  -> Seq D0'9
  -> D0'9
  -> Seq D0'9
  -> Seq (D0'9, Seq D0'9)
  -> BG5RadPer
fBG5NovemRadPer d0 ds1 d2 ds3 gs4
  = BG5NovemRadPer d0 (D0'9'Seq ds1) fGrpRadPer d2 (D0'9'Seq ds3)
    (DigitGroupRadPer'Seq (fmap (uncurry fDigitGroupRadPer) gs4))

fBG5ZeroRadPer
  :: Positive
  -- ^ Number of leading zeroes
  -> BG6RadPer
  -> BG5RadPer
fBG5ZeroRadPer pos b6 = BG5ZeroRadPer sZero rest b6
  where
    rest = case Positive.prev pos of
      Nothing -> fZeroes NonNegative.zero
      Just pos' -> fZeroes (NonNegative.c'NonNegative'Positive pos')

fBG4DigitRadPer
  :: D0'9
  -> Seq D0'9
  -> Seq (D0'9, Seq D0'9)
  -> BG4RadPer
fBG4DigitRadPer d1 ds2 dss
  = BG4DigitRadPer d1 (D0'9'Seq ds2)
    (DigitGroupRadPer'Seq (fmap (uncurry fDigitGroupRadPer) dss))

fBG4NilRadPer :: BG4RadPer
fBG4NilRadPer = BG4NilRadPer

fBG3RadixRadPer :: BG4RadPer -> BG3RadPer
fBG3RadixRadPer b4 = BG3RadixRadPer sRadixPer b4

fBG3NilRadPer :: BG3RadPer
fBG3NilRadPer = BG3NilRadPer

fBG1GroupOnLeftRadPer
  :: D0'9
  -> Seq D0'9
  -> Seq (D0'9, Seq D0'9)
  -> BG3RadPer
  -> BG1RadPer
fBG1GroupOnLeftRadPer d0 ds1 gs2 b3 = BG1GroupOnLeftRadPer fGrpRadPer d0
  (D0'9'Seq ds1)
  (DigitGroupRadPer'Seq (fmap (uncurry fDigitGroupRadPer) gs2))
  b3

fBG1GroupOnRightRadPer
  :: D0'9
  -> Seq D0'9
  -> D0'9
  -> Seq D0'9
  -> Seq (D0'9, Seq D0'9)
  -> BG1RadPer
fBG1GroupOnRightRadPer d1 ds2 d4 ds5 g6
  = BG1GroupOnRightRadPer sRadixPer d1 (D0'9'Seq ds2) fGrpRadPer
  d4 (D0'9'Seq ds5)
  (DigitGroupRadPer'Seq (fmap (uncurry fDigitGroupRadPer) g6))

fBGGreaterThanOneRadPer
  :: D1'9
  -> Seq D0'9
  -> BG1RadPer
  -> BrimGroupedRadPer
fBGGreaterThanOneRadPer d0 ds1 b1
  = BGGreaterThanOneRadPer d0 (D0'9'Seq ds1) b1

fBGLessThanOneRadPer
  :: BG5RadPer
  -> BrimGroupedRadPer
fBGLessThanOneRadPer b5
  = BGLessThanOneRadPer (Zero'Maybe (Just sZero)) sRadixPer b5

-- # Dates. Use a hyphen as separator.

fDateSep :: DateSep
fDateSep = DateSep'Hyphen sHyphen

fJan :: Days31 -> MonthDay
fJan d = Jan sZero sOne fDateSep d

fFeb :: Days28 -> MonthDay
fFeb d = Feb sZero sTwo fDateSep d

fMar :: Days31 -> MonthDay
fMar d = Mar sZero sThree fDateSep d

fApr :: Days30 -> MonthDay
fApr d = Apr sZero sFour fDateSep d

fMay :: Days31 -> MonthDay
fMay d = May sZero sFive fDateSep d

fJun :: Days30 -> MonthDay
fJun d = Jun sZero sSix fDateSep d

fJul :: Days31 -> MonthDay
fJul d = Jul sZero sSeven fDateSep d

fAug :: Days31 -> MonthDay
fAug d = Aug sZero sEight fDateSep d

fSep :: Days30 -> MonthDay
fSep d = Sep sZero sNine fDateSep d

fOct :: Days31 -> MonthDay
fOct d = Oct sOne sZero fDateSep d

fNov :: Days30 -> MonthDay
fNov d = Nov sOne sOne fDateSep d

fDec :: Days31 -> MonthDay
fDec d = Dec sOne sTwo fDateSep d

fNonLeapDay :: Year -> MonthDay -> NonLeapDay
fNonLeapDay y md = NonLeapDay y fDateSep md

fLeapDay :: LeapYear -> LeapDay
fLeapDay ly = LeapDay ly fDateSep sZero sTwo fDateSep sTwo sNine

fComment :: Seq Char -> Maybe Comment
fComment = fmap f . sequence . fmap (Lens.preview _CommentChar)
  where
    f cs = Comment sHash (CommentChar'Seq cs) sNewline

fTime
  :: Hours
  -> (D0'5, D0'9)
  -- ^ Minutes
  -> Maybe (D0'5, D0'9)
  -- ^ Seconds
  -> Time
fTime h (m1, m2) s = Time h sColon (Minutes (N0'59 m1 m2)) s'
  where
    s' = case s of
      Nothing -> ColonSeconds'Maybe Nothing
      Just (s1, s2) -> ColonSeconds'Maybe (Just (ColonSeconds sColon
        (Seconds (N0'59 s1 s2))))

fZone
  :: Pole
  -- ^ Positive or negative
  -> D0'2
  -> D0'3
  -> D0'9
  -> D0'9
  -> Zone
fZone p d0 d1 d2 d3 = Zone sBacktick
  (ZoneHrsMins pm d0 d1 d2 d3)
  where
    pm | p == positive = PluMin'Plus sPlus
       | otherwise = PluMin'Minus sMinus

space :: White'Seq
space = White'Seq . Seq.singleton
  $ White'Space sSpace

newline :: White'Seq
newline = White'Seq . Seq.singleton . White'Newline $ sNewline

noSpace :: White'Seq
noSpace = White'Seq Seq.empty

fCommaTree :: Tree -> CommaTree
fCommaTree tree = CommaTree sComma space tree noSpace

fCommaTree'Seq :: Seq Tree -> CommaTree'Seq
fCommaTree'Seq = CommaTree'Seq . fmap fCommaTree

fForest :: Tree -> Seq Tree -> Forest
fForest t1 ts = Forest t1 space (fCommaTree'Seq ts)

fBracketedForest :: Forest -> BracketedForest
fBracketedForest f
  = BracketedForest sOpenSquare space f sCloseSquare newline

fTree :: Scalar -> Maybe Forest -> Tree
fTree sc mayForest = TreeScalarFirst sc (BracketedForest'Maybe may)
  where
    may = fmap fBracketedForest mayForest

fDateTimeZone
  :: Date
  -> Time
  -> Zone
  -> Forest
fDateTimeZone date time zone = fForest dateTree [timeTree, zoneTree]
  where
    dateTree = fTree (Scalar'Date date) Nothing
    timeTree = undefined
    zoneTree = undefined
