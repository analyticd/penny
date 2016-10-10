{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- | Copperize takes other data types, both from Penny and from
-- other libraries, and turns them into Copper data types.
--
-- Functions and values in this module do not perform any formatting.
-- They present the information in the simplest way possible that is
-- type-safe.  Humans may find this output barely readable.  Not
-- performing any formatting keeps the values and functions in this
-- module simple.  To format the output nicely for humans, see
-- "Penny.Copper.Formatter".
module Penny.Copper.Copperize where

import Accuerr (Accuerr(AccFailure, AccSuccess))
import qualified Accuerr
import Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import Data.Foldable (toList)
import Data.Maybe (fromJust)
import qualified Data.OFX as OFX
import Data.Sequence (Seq, (<|))
import qualified Data.Sequence as Seq
import Data.Sequence.NonEmpty (NonEmptySeq)
import qualified Data.Sequence.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as X
import qualified Data.Time as Time

import qualified Penny.Amount as Amount
import Penny.Arrangement
import qualified Penny.Commodity as Commodity
import Penny.Copper.Grouping
import Penny.Copper.Optics
import Penny.Copper.PriceParts
import Penny.Copper.Terminalizers
import Penny.Copper.Tracompri
import Penny.Copper.Types
import Penny.Decimal
import Penny.Ents
import qualified Penny.Fields as Fields
import Penny.NonNegative (NonNegative)
import qualified Penny.NonNegative as NN
import qualified Penny.NonZero as NZ
import Penny.Polar
import Penny.Positive (Positive)
import qualified Penny.Positive as Pos
import Penny.Rep
import Penny.SeqUtil
import qualified Penny.Tranche as Tranche
import qualified Penny.Transaction as Txn
import qualified Penny.Troika as Troika


cZero :: Zero Char ()
cZero = Zero ('0', ())

cOne :: One Char ()
cOne = One ('1', ())

cTwo :: Two Char ()
cTwo = Two ('2', ())

cThree :: Three Char ()
cThree = Three ('3', ())

cFour :: Four Char ()
cFour = Four ('4', ())

cFive :: Five Char ()
cFive = Five ('5', ())

cSix :: Six Char ()
cSix = Six ('6', ())

cSeven :: Seven Char ()
cSeven = Seven ('7', ())

cEight :: Eight Char ()
cEight = Eight ('8', ())

cNine :: Nine Char ()
cNine = Nine ('9', ())

c'D0'9'Int :: Integral a => a -> Maybe (D0'9 Char ())
c'D0'9'Int x = case x of
  0 -> Just $ D0'9'Zero (Zero ('0', ()))
  1 -> Just $ D0'9'One (One ('1', ()))
  2 -> Just $ D0'9'Two (Two ('2', ()))
  3 -> Just $ D0'9'Three (Three ('3', ()))
  4 -> Just $ D0'9'Four (Four ('4', ()))
  5 -> Just $ D0'9'Five (Five ('5', ()))
  6 -> Just $ D0'9'Six (Six ('6', ()))
  7 -> Just $ D0'9'Seven (Seven ('7', ()))
  8 -> Just $ D0'9'Eight (Eight ('8', ()))
  9 -> Just $ D0'9'Nine (Nine ('9', ()))
  _ -> Nothing

c'D1'9'Int :: Integral a => a -> Maybe (D1'9 Char ())
c'D1'9'Int x = case x of
  1 -> Just $ D1'9'One (One ('1', ()))
  2 -> Just $ D1'9'Two (Two ('2', ()))
  3 -> Just $ D1'9'Three (Three ('3', ()))
  4 -> Just $ D1'9'Four (Four ('4', ()))
  5 -> Just $ D1'9'Five (Five ('5', ()))
  6 -> Just $ D1'9'Six (Six ('6', ()))
  7 -> Just $ D1'9'Seven (Seven ('7', ()))
  8 -> Just $ D1'9'Eight (Eight ('8', ()))
  9 -> Just $ D1'9'Nine (Nine ('9', ()))
  _ -> Nothing

c'D0'8'Int :: Integral a => a -> Maybe (D0'8 Char ())
c'D0'8'Int x = case x of
  0 -> Just $ D0'8'Zero (Zero ('0', ()))
  1 -> Just $ D0'8'One (One ('1', ()))
  2 -> Just $ D0'8'Two (Two ('2', ()))
  3 -> Just $ D0'8'Three (Three ('3', ()))
  4 -> Just $ D0'8'Four (Four ('4', ()))
  5 -> Just $ D0'8'Five (Five ('5', ()))
  6 -> Just $ D0'8'Six (Six ('6', ()))
  7 -> Just $ D0'8'Seven (Seven ('7', ()))
  8 -> Just $ D0'8'Eight (Eight ('8', ()))
  _ -> Nothing

c'D0'1'Int :: Integral a => a -> Maybe (D0'1 Char ())
c'D0'1'Int x = case x of
  0 -> Just $ D0'1'Zero cZero
  1 -> Just $ D0'1'One cOne
  _ -> Nothing

c'D0'3'Int :: Integral a => a -> Maybe (D0'3 Char ())
c'D0'3'Int x = case x of
  0 -> Just $ D0'3'Zero (Zero ('0', ()))
  1 -> Just $ D0'3'One (One ('1', ()))
  2 -> Just $ D0'3'Two (Two ('2', ()))
  3 -> Just $ D0'3'Three (Three ('3', ()))
  _ -> Nothing

c'D0'5'Int :: Integral a => a -> Maybe (D0'5 Char ())
c'D0'5'Int x = case x of
  0 -> Just $ D0'5'Zero (Zero ('0', ()))
  1 -> Just $ D0'5'One (One ('1', ()))
  2 -> Just $ D0'5'Two (Two ('2', ()))
  3 -> Just $ D0'5'Three (Three ('3', ()))
  4 -> Just $ D0'5'Four (Four ('4', ()))
  5 -> Just $ D0'5'Five (Five ('5', ()))
  _ -> Nothing

-- # Dates and times

c'Days28'Int :: Integral a => a -> Maybe (Days28 Char ())
c'Days28'Int x = d1to9 <|> d10to19 <|> d20to28
  where
    d1to9 = do
      d <- c'D1'9'Int x
      return $ D28'1to9 cZero d
    d10to19 = do
      d <- c'D0'9'Int (x - 10)
      return $ D28'10to19 cOne d
    d20to28 = do
      d <- c'D0'8'Int (x - 20)
      return $ D28'20to28 cTwo d

c'Days30'Int :: Integral a => a -> Maybe (Days30 Char ())
c'Days30'Int x = d28 <|> d29 <|> d30
  where
    d28 = fmap D30'28 (c'Days28'Int x)
    d29 | x == 29 = Just $ D30'29 cTwo cNine
        | otherwise = Nothing
    d30 | x == 30 = Just $ D30'30 cThree cZero
        | otherwise = Nothing

c'Days31'Int :: Integral a => a -> Maybe (Days31 Char ())
c'Days31'Int x = d30 <|> d31
  where
    d30 = fmap D31'30 $ c'Days30'Int x
    d31 | x == 31 = Just $ D31'31 cThree cOne
        | otherwise = Nothing

c'Year'Int :: Integral a => a -> Maybe (Year Char ())
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

c'Mod4'Int :: Integral a => a -> Maybe (Mod4 Char ())
c'Mod4'Int x = case x of
  { 4 -> Just $ L04 cZero cFour; 8 -> Just $ L08 cZero cEight;
    12 -> Just $ L12 cOne cTwo;
    16 -> Just $ L16 cOne cSix; 20 -> Just $ L20 cTwo cZero;
    24 -> Just $ L24 cTwo cFour;
    28 -> Just $ L28 cTwo cEight; 32 -> Just $ L32 cThree cTwo;
    36 -> Just $ L36 cThree cSix;
    40 -> Just $ L40 cFour cZero; 44 -> Just $ L44 cFour cFour;
    48 -> Just $ L48 cFour cEight;
    52 -> Just $ L52 cFive cTwo; 56 -> Just $ L56 cFive cSix;
    60 -> Just $ L60 cSix cZero;
    64 -> Just $ L64 cSix cFour; 68 -> Just $ L68 cSix cEight;
    72 -> Just $ L72 cSeven cTwo;
    76 -> Just $ L76 cSeven cSix; 80 -> Just $ L80 cEight cZero;
    84 -> Just $ L84 cEight cFour;
    88 -> Just $ L88 cEight cEight; 92 -> Just $ L92 cNine cTwo;
    96 -> Just $ L96 cNine cSix;
    _ -> Nothing }

c'CenturyLeapYear'Int :: Integral a => a -> Maybe (CenturyLeapYear Char ())
c'CenturyLeapYear'Int x
  | x == 0 = Just $ LeapYear0 cZero cZero cZero cZero
  | rm == 0 = do
      m4 <- c'Mod4'Int dv
      return $ LeapYearMod4 m4 cZero cZero
  | otherwise = Nothing
  where
    (dv, rm) = x `divMod` 100

c'NonCenturyLeapYear'Int :: Integral a => a -> Maybe (NonCenturyLeapYear Char ())
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

c'LeapYear'Int :: Integral a => a -> Maybe (LeapYear Char ())
c'LeapYear'Int x = fmap LeapYear'CenturyLeapYear (c'CenturyLeapYear'Int x)
    <|> fmap LeapYear'NonCenturyLeapYear (c'NonCenturyLeapYear'Int x)

c'N0'19'Int :: Integral a => a -> Maybe (N0'19 Char ())
c'N0'19'Int x = d10'19 <|> d0'9
  where
    d10'19 = do
      d1 <- c'D0'9'Int (x - 10)
      return (N0'19 (D0'1'Opt (Just (D0'1'One cOne))) d1)
    d0'9 = do
      d0'9 <- c'D0'9'Int x
      return $ N0'19 (D0'1'Opt Nothing) d0'9

c'N20'23'Int :: Integral a => a -> Maybe (N20'23 Char ())
c'N20'23'Int x = do
  d1 <- c'D0'3'Int $ x - 20
  return $ N20'23 cTwo d1

c'Hours'Int :: Integral a => a -> Maybe (Hours Char ())
c'Hours'Int i = Hours'N0'19 <$> c'N0'19'Int i
    <|> Hours'N20'23 <$> c'N20'23'Int i

c'N0'59'Int :: Integral a => a -> Maybe (N0'59 Char ())
c'N0'59'Int x = do
  let (r0, intDigit0) = x `divMod` 10
  d0 <- c'D0'9'Int intDigit0
  d1 <- c'D0'5'Int r0
  return $ N0'59 d1 d0

c'Minutes'Int :: Integral a => a -> Maybe (Minutes Char ())
c'Minutes'Int = fmap Minutes . c'N0'59'Int

c'Seconds'Int :: Integral a => a -> Maybe (Seconds Char ())
c'Seconds'Int = fmap Seconds . c'N0'59'Int

cThinSpace :: ThinSpace Char ()
cThinSpace = ThinSpace ('\x2009', ())

cUnderscore :: Underscore Char ()
cUnderscore = Underscore ('_', ())

cPeriod :: Period Char ()
cPeriod = Period ('.', ())

cComma :: Comma Char ()
cComma = Comma (',', ())

cRadixCom :: RadixCom Char ()
cRadixCom = RadixCom (',', ())

cRadixPer :: RadixPer Char ()
cRadixPer = RadixPer ('.', ())

cHyphen :: Hyphen Char ()
cHyphen = Hyphen ('-', ())

cSlash :: Slash Char ()
cSlash = Slash ('/', ())

cNewline :: Newline Char ()
cNewline = Newline ('\n', ())

cHash :: Hash Char ()
cHash = Hash ('#', ())

cColon :: Colon Char ()
cColon = Colon (':', ())

cPlus :: Plus Char ()
cPlus = Plus ('+', ())

cMinus :: Minus Char ()
cMinus = Minus ('-', ())

cBacktick :: Backtick Char ()
cBacktick = Backtick ('`', ())

cDoubleQuote :: DoubleQuote Char ()
cDoubleQuote = DoubleQuote ('"', ())

cBackslash :: Backslash Char ()
cBackslash = Backslash ('\\', ())

cSpace :: Space Char ()
cSpace = Space (' ', ())

cTab :: Tab Char ()
cTab = Tab ('\t', ())

cLessThan :: LessThan Char ()
cLessThan = LessThan ('<', ())

cGreaterThan :: GreaterThan Char ()
cGreaterThan = GreaterThan ('>', ())

cOpenSquare :: OpenSquare Char ()
cOpenSquare = OpenSquare ('[', ())

cCloseSquare :: CloseSquare Char ()
cCloseSquare = CloseSquare (']', ())

cOpenCurly :: OpenCurly Char ()
cOpenCurly = OpenCurly ('{', ())

cCloseCurly :: CloseCurly Char ()
cCloseCurly = CloseCurly ('}', ())

cSemicolon :: Semicolon Char ()
cSemicolon = Semicolon (';', ())

cAtSign :: AtSign Char ()
cAtSign = AtSign ('@', ())

cOpenParen :: OpenParen Char ()
cOpenParen = OpenParen ('(', ())

cCloseParen :: CloseParen Char ()
cCloseParen = CloseParen (')', ())

cCommentChar :: Char -> Maybe (CommentChar Char ())
cCommentChar c = Lens.preview _CommentChar (c, ())

-- | Converts a 'Text' to a 'CommentChar'Star'.  Fails if any
-- character cannot be placed into a 'CommentChar' and returns a
-- 'Left' with the bad character.
cCommentChar'Star
  :: Text
  -> Accuerr (NonEmptySeq Char) (CommentChar'Star Char ())
cCommentChar'Star
  = fmap CommentChar'Star . traverse f . Seq.fromList . X.unpack
  where
    f c = case cCommentChar c of
      Nothing -> AccFailure . NE.singleton $ c
      Just r -> AccSuccess r

cComment :: Text -> Accuerr (NonEmptySeq Char) (Comment Char ())
cComment txt
  = Comment
  <$> pure cHash
  <*> cCommentChar'Star txt
  <*> pure cNewline

cWhite'Space :: White Char ()
cWhite'Space = White'Space cSpace

cWhite'Tab :: White Char ()
cWhite'Tab = White'Tab cTab

cWhite'Newline :: White Char ()
cWhite'Newline = White'Newline cNewline

cWhite'Star :: White'Star Char ()
cWhite'Star = mempty

cNonNegative :: NonNegative -> Seq (D0'9 Char ())
cNonNegative = go Seq.empty . NN.c'Integer'NonNegative
  where
    go acc int =
      let (rem, thisDigit) = int `divMod` 10
          thisDig = case thisDigit of
            0 -> D0'9'Zero cZero
            1 -> D0'9'One cOne
            2 -> D0'9'Two cTwo
            3 -> D0'9'Three cThree
            4 -> D0'9'Four cFour
            5 -> D0'9'Five cFive
            6 -> D0'9'Six cSix
            7 -> D0'9'Seven cSeven
            8 -> D0'9'Eight cEight
            9 -> D0'9'Nine cNine
            _ -> error "cNonNegative: error 1"
          res = thisDig `Lens.cons` acc
      in if rem == 0 then res else go res rem

-- Numbers

-- | Transform a 'Positive' into its component digits.
positiveDigits
  :: Positive
  -> (D1'9 Char (), Seq (D0'9 Char ()))
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

repDigitsRadCom
  :: (D1'9 Char (), Seq (D0'9 Char ()))
  -- ^ Significand
  -> NonNegative
  -- ^ Exponent
  -> BrimUngroupedRadCom Char ()
repDigitsRadCom (d1, dr) expt
  = case NN.diff (NN.next $ NN.length dr) expt of
      NN.Equal -> BULessThanOneRadCom (Zero'Opt $ Just cZero)
        rdx (Zero'Star Seq.empty) d1 (D0'9'Star dr)
      NN.LeftBiggerBy l -> BUGreaterThanOneRadCom d1 (D0'9'Star leftDigs)
        (RadixComDigits'Opt rightDigs)
        where
          (leftDigs, rightDigs) = case Pos.prev l of
            Nothing -> (Seq.empty, Just (RadixComDigits rdx (D0'9'Star dr)))
            Just c -> (beg,
              Just (RadixComDigits rdx (D0'9'Star end)))
              where
                (beg, end) = Seq.splitAt (integerToInt $ Pos.c'Integer'Positive c) dr
      NN.RightBiggerBy r -> BULessThanOneRadCom (Zero'Opt $ Just cZero)
        rdx (Zero'Star zs) d1 (D0'9'Star dr)
        where
          zs = flip Seq.replicate cZero
            . integerToInt . Pos.c'Integer'Positive $ r
  where
    rdx = cRadixCom

repDigitsRadPer
  :: (D1'9 Char (), Seq (D0'9 Char ()))
  -- ^ Significand
  -> NonNegative
  -- ^ Exponent
  -> BrimUngroupedRadPer Char ()
repDigitsRadPer (d1, dr) expt
  = case NN.diff (NN.next $ NN.length dr) expt of
      NN.Equal -> BULessThanOneRadPer (Zero'Opt $ Just cZero)
        rdx (Zero'Star Seq.empty) d1 (D0'9'Star dr)
      NN.LeftBiggerBy l -> BUGreaterThanOneRadPer d1 (D0'9'Star leftDigs)
        (RadixPerDigits'Opt rightDigs)
        where
          (leftDigs, rightDigs) = case Pos.prev l of
            Nothing -> (Seq.empty, Just (RadixPerDigits rdx (D0'9'Star dr)))
            Just c -> (beg,
              Just (RadixPerDigits rdx (D0'9'Star end)))
              where
                (beg, end) = Seq.splitAt (integerToInt $ Pos.c'Integer'Positive c) dr
      NN.RightBiggerBy r -> BULessThanOneRadPer (Zero'Opt $ Just cZero)
        rdx (Zero'Star zs) d1 (D0'9'Star dr)
        where
          zs = flip Seq.replicate cZero
            . integerToInt . Pos.c'Integer'Positive $ r
  where
    rdx = cRadixPer

repUngroupedDecZeroRadCom
  :: DecZero
  -> NilUngroupedRadCom Char ()
repUngroupedDecZeroRadCom (Exponential () expt) = NUZeroRadCom cZero
  (RadixZeroesRadCom'Opt mayRdx)
  where
    rdx = cRadixCom
    mayRdx
      | expt == NN.zero = Nothing
      | otherwise = Just (RadixZeroesRadCom rdx (Zero'Star zs))
      where
        zs = Seq.replicate (integerToInt . NN.c'Integer'NonNegative $ expt) cZero

repUngroupedDecZeroRadPer
  :: DecZero
  -> NilUngroupedRadPer Char ()
repUngroupedDecZeroRadPer (Exponential () expt) = NUZeroRadPer cZero
  (RadixZeroesRadPer'Opt mayRdx)
  where
    rdx = cRadixPer
    mayRdx
      | expt == NN.zero = Nothing
      | otherwise = Just (RadixZeroesRadPer rdx (Zero'Star zs))
      where
        zs = Seq.replicate (integerToInt . NN.c'Integer'NonNegative $ expt) cZero

repUngroupedDecPositiveRadCom
  :: DecPositive
  -> BrimUngroupedRadCom Char ()
repUngroupedDecPositiveRadCom (Exponential sig expt)
  = repDigitsRadCom (positiveDigits sig) expt

repUngroupedDecPositiveRadPer
  :: DecPositive
  -> BrimUngroupedRadPer Char ()
repUngroupedDecPositiveRadPer (Exponential sig expt)
  = repDigitsRadPer (positiveDigits sig) expt

repUngroupedDecNonZeroRadCom
  :: DecNonZero
  -> (BrimUngroupedRadCom Char (), Pole)
repUngroupedDecNonZeroRadCom nz = (repUngroupedDecPositiveRadCom dp, sgn)
  where
    (dp, sgn) = stripNonZeroSign nz

repUngroupedDecNonZeroRadPer
  :: DecNonZero
  -> (BrimUngroupedRadPer Char (), Pole)
repUngroupedDecNonZeroRadPer nz = (repUngroupedDecPositiveRadPer dp, sgn)
  where
    (dp, sgn) = stripNonZeroSign nz

repUngroupedDecimalRadCom
  :: Decimal
  -> Moderated (NilUngroupedRadCom Char ()) (BrimUngroupedRadCom Char ())
repUngroupedDecimalRadCom d = case stripDecimalSign d of
  Left zero -> Moderate (repUngroupedDecZeroRadCom zero)
  Right (pos, pm) ->
    Extreme (Polarized (repUngroupedDecPositiveRadCom pos) pm)

repUngroupedDecimalRadPer
  :: Decimal
  -> Moderated (NilUngroupedRadPer Char ()) (BrimUngroupedRadPer Char ())
repUngroupedDecimalRadPer d = case stripDecimalSign d of
  Left zero -> Moderate (repUngroupedDecZeroRadPer zero)
  Right (pos, pm) ->
    Extreme (Polarized (repUngroupedDecPositiveRadPer pos) pm)

repUngroupedDecUnsignedRadCom
  :: DecUnsigned
  -> Either (NilUngroupedRadCom Char ()) (BrimUngroupedRadCom Char ())
repUngroupedDecUnsignedRadCom uns = case decomposeDecUnsigned uns of
  Left z -> Left (repUngroupedDecZeroRadCom z)
  Right p -> Right (repUngroupedDecPositiveRadCom p)

repUngroupedDecUnsignedRadPer
  :: DecUnsigned
  -> Either (NilUngroupedRadPer Char ()) (BrimUngroupedRadPer Char ())
repUngroupedDecUnsignedRadPer uns = case decomposeDecUnsigned uns of
  Left z -> Left (repUngroupedDecZeroRadPer z)
  Right p -> Right (repUngroupedDecPositiveRadPer p)

repDecimal
  :: Either (Maybe (GrpRadCom Char ())) (Maybe (GrpRadPer Char ()))
  -- ^ Determines which radix is used.  If you also supply a grouping
  -- character, 'repDecimal' will try to group the 'Decimal' as well.
  -- Grouping will fail if the absolute value of the 'Decimal' is less
  -- than @1000@.  In that case the 'Decimal' will be represented without
  -- grouping.
  -> Decimal
  -> RepAnyRadix
repDecimal ei d = case ei of
  Left mayRadCom -> Left $ case mayRadCom of
    Nothing -> case repUngroupedDecimalRadCom d of
      Moderate nilU -> Moderate $ NilRadCom'NilUngroupedRadCom nilU
      Extreme (Polarized brimU side) -> Extreme
        (Polarized (BrimRadCom'BrimUngroupedRadCom brimU) side)
    Just grpr -> case repUngroupedDecimalRadCom d of
      Moderate nilU -> Moderate $ NilRadCom'NilUngroupedRadCom nilU
      Extreme (Polarized brimU side) ->
        case groupBrimUngroupedRadCom grpr brimU of
          Nothing -> Extreme
            (Polarized (BrimRadCom'BrimUngroupedRadCom brimU) side)
          Just grouped -> Extreme
            (Polarized (BrimRadCom'BrimGroupedRadCom grouped) side)

  Right mayRadPer -> Right $ case mayRadPer of
    Nothing -> case repUngroupedDecimalRadPer d of
      Moderate nilU -> Moderate $ NilRadPer'NilUngroupedRadPer nilU
      Extreme (Polarized brimU side) -> Extreme
        (Polarized (BrimRadPer'BrimUngroupedRadPer brimU) side)
    Just grpr -> case repUngroupedDecimalRadPer d of
      Moderate nilU -> Moderate $ NilRadPer'NilUngroupedRadPer nilU
      Extreme (Polarized brimU side) ->
        case groupBrimUngroupedRadPer grpr brimU of
          Nothing -> Extreme
            (Polarized (BrimRadPer'BrimUngroupedRadPer brimU) side)
          Just grouped -> Extreme
            (Polarized (BrimRadPer'BrimGroupedRadPer grouped) side)

cInteger
  :: Integer
  -> WholeAny Char ()
cInteger i = case NZ.c'NonZero'Integer i of
  Nothing -> WholeAny'Zero cZero
  Just nz -> WholeAny'WholeNonZero (WholeNonZero pm d1 (D0'9'Star ds))
    where
      pm | p == negative = PluMin'Opt (Just . PluMin'Minus $ cMinus)
         | otherwise = PluMin'Opt Nothing
        where
          p = Lens.view NZ.pole nz
      (d1, ds) = positiveDigits . NZ.c'Positive'NonZero $ nz

-- | Provide a simple ungrouped string for a decimal.
displayDecimalAsQty
  :: Decimal
  -> ShowS
displayDecimalAsQty d = (toList (sideChar : ' ' : rest) ++)
  where
    sideChar = case integerPole . _coefficient $ d of
      Nothing -> ' '
      Just v
        | v == debit -> '<'
        | otherwise -> '>'
    rest = fmap fst . toList $ case repUngroupedDecimalRadPer d of
      Moderate nu -> t'NilUngroupedRadPer nu
      Extreme (Polarized bu _) -> t'BrimUngroupedRadPer bu

-- # Strings

cNonEscapedChar :: Char -> Maybe (NonEscapedChar Char ())
cNonEscapedChar c = Lens.preview _NonEscapedChar (c, ())

cEscSeq :: Char -> Maybe (EscSeq Char ())
cEscSeq c
  | c == '\\' = sq (EscPayload'Backslash cBackslash)
  | c == '\n' = sq (EscPayload'Newline cNewline)
  | c == '"' = sq (EscPayload'DoubleQuote cDoubleQuote)
  | otherwise = Nothing
  where
    sq p = Just $ EscSeq cBackslash p

cQuotedChar :: Char -> (QuotedChar Char ())
cQuotedChar c = case cEscSeq c of
  Nothing -> QuotedChar'NonEscapedChar (NonEscapedChar (c, ()))
  Just s -> QuotedChar'EscSeq s

cQuotedChar'Star :: Foldable c => c Char -> QuotedChar'Star Char ()
cQuotedChar'Star = QuotedChar'Star . Seq.fromList . fmap cQuotedChar . toList

cQuotedString :: Foldable c => c Char -> QuotedString Char ()
cQuotedString x = QuotedString cDoubleQuote (cQuotedChar'Star x) cDoubleQuote

cUnquotedStringNonDigitChar
  :: Char
  -> Maybe (UnquotedStringNonDigitChar Char ())
cUnquotedStringNonDigitChar c
  = Lens.preview _UnquotedStringNonDigitChar (c, ())

cD0'9 :: Char -> Maybe (D0'9 Char ())
cD0'9 c
  | c == '0' = Just (D0'9'Zero cZero)
  | c == '1' = Just (D0'9'One cOne)
  | c == '2' = Just (D0'9'Two cTwo)
  | c == '3' = Just (D0'9'Three cThree)
  | c == '4' = Just (D0'9'Four cFour)
  | c == '5' = Just (D0'9'Five cFive)
  | c == '6' = Just (D0'9'Six cSix)
  | c == '7' = Just (D0'9'Seven cSeven)
  | c == '8' = Just (D0'9'Eight cEight)
  | c == '9' = Just (D0'9'Nine cNine)
  | otherwise = Nothing

cD0'9'Star :: Traversable c => c Char -> Maybe (D0'9'Star Char ())
cD0'9'Star
  = fmap (D0'9'Star . Seq.fromList . toList)
  . sequence
  . fmap cD0'9

cUnquotedStringNonFirstChar
  :: Char
  -> Maybe (UnquotedStringNonFirstChar Char ())
cUnquotedStringNonFirstChar c
  = UnquotedStringNonFirstChar'UnquotedStringNonDigitChar
      <$> cUnquotedStringNonDigitChar c
  <|> UnquotedStringNonFirstChar'D0'9
      <$> cD0'9 c

cUnquotedStringNonFirstChar'Star
  :: Traversable c
  => c Char
  -> Maybe (UnquotedStringNonFirstChar'Star Char ())
cUnquotedStringNonFirstChar'Star
  = fmap (UnquotedStringNonFirstChar'Star . Seq.fromList . toList)
  . sequence
  . fmap cUnquotedStringNonFirstChar

cUnquotedString :: Seq Char -> Maybe (UnquotedString Char ())
cUnquotedString chars = do
  let (digits, rest) = getFirstDigits chars
  (firstNonDigitChar, rest') <- getFirstNonDigitChar rest
  rest'' <- cUnquotedStringNonFirstChar'Star rest'
  return $ UnquotedString digits firstNonDigitChar rest''
  where
    getFirstDigits sq =
      let (digs, rest) = convertHead cD0'9 sq
      in (D0'9'Star digs, rest)
    getFirstNonDigitChar sq = do
      (first, rest) <- Lens.uncons sq
      nonDigit <- cUnquotedStringNonDigitChar first
      return (nonDigit, rest)

cString :: Seq Char -> Either (UnquotedString Char ()) (QuotedString Char ())
cString cs = case cUnquotedString cs of
  Just s -> Left s
  Nothing -> Right . cQuotedString $ cs

cUnquotedStringNonDigitChar'Plus
  :: Seq Char
  -> Maybe (UnquotedStringNonDigitChar'Plus Char ())
cUnquotedStringNonDigitChar'Plus sq = do
  (x, xs) <- Lens.uncons sq
  x' <- cUnquotedStringNonDigitChar x
  xs' <- sequence . fmap cUnquotedStringNonDigitChar $ xs
  return $ UnquotedStringNonDigitChar'Plus (NE.NonEmptySeq x' xs')

cCommodity :: Commodity.Commodity -> Commodity Char ()
cCommodity cy = case cUnquotedStringNonDigitChar'Plus sq of
  Nothing -> Commodity'QuotedCommodity (QuotedCommodity (cQuotedString sq))
  Just us -> Commodity'UnquotedCommodity (UnquotedCommodity us)
  where
    sq = Seq.fromList . X.unpack $ cy


-- | Grouper of thin space
cGrouper :: Grouper Char ()
cGrouper = Grouper'ThinSpace cThinSpace

-- | Period grouper
cGrpRadCom :: GrpRadCom Char ()
cGrpRadCom = GrpRadCom'Period cPeriod

-- | Comma grouper
cGrpRadPer :: GrpRadPer Char ()
cGrpRadPer = GrpRadPer'Comma cComma

cDigitGroupRadCom
  :: D0'9 Char ()
  -> Seq (D0'9 Char ())
  -> DigitGroupRadCom Char ()
cDigitGroupRadCom d1 ds
  = DigitGroupRadCom cGrpRadCom d1 (D0'9'Star ds)

cDigitGroupRadPer
  :: D0'9 Char ()
  -> Seq (D0'9 Char ())
  -> DigitGroupRadPer Char ()
cDigitGroupRadPer d1 ds
  = DigitGroupRadPer cGrpRadPer d1 (D0'9'Star ds)

-- | Returns a number of zeroes the same as the 'NonNegative'.
cZeroes :: NonNegative -> Zero'Star Char ()
cZeroes = Zero'Star . go Seq.empty
  where
    go acc nn = case NN.prev nn of
      Nothing -> acc
      Just p -> go (Lens.cons cZero acc) p

cRadixZeroesRadCom :: NonNegative -> RadixZeroesRadCom Char ()
cRadixZeroesRadCom nn = RadixZeroesRadCom cRadixCom (cZeroes nn)

cRadixZeroesRadPer :: NonNegative -> RadixZeroesRadPer Char ()
cRadixZeroesRadPer nn = RadixZeroesRadPer cRadixPer (cZeroes nn)

cZeroGroupRadCom :: Positive -> ZeroGroupRadCom Char ()
cZeroGroupRadCom pos
  = ZeroGroupRadCom cGrpRadCom cZero (cZeroes rest)
  where
    rest = case Pos.prev pos of
      Nothing -> NN.zero
      Just p -> NN.c'NonNegative'Positive p

cZeroGroupRadPer :: Positive -> ZeroGroupRadPer Char ()
cZeroGroupRadPer pos
  = ZeroGroupRadPer cGrpRadPer cZero (cZeroes rest)
  where
    rest = case Pos.prev pos of
      Nothing -> NN.zero
      Just p -> NN.c'NonNegative'Positive p

-- | Has a leading zero and period for grouper.
cNilGroupedRadCom
  :: Positive
  -- ^ Number of leading zeroes before first grouping character
  -> ZeroGroupRadCom Char ()
  -> Seq (ZeroGroupRadCom Char ())
  -> NilGroupedRadCom Char ()
cNilGroupedRadCom lead g1 gs = NilGroupedRadCom mz0 r1 z2 z3 zg4
  where
    mz0 = Zero'Opt (Just cZero)
    r1 = cRadixCom
    z2 = cZero
    z3 = case Pos.prev lead of
      Nothing -> Zero'Star Seq.empty
      Just p -> cZeroes (NN.c'NonNegative'Positive p)
    zg4 = ZeroGroupRadCom'Plus $ NE.NonEmptySeq g1 gs

-- | Has a leading zero and comma for grouper.
cNilGroupedRadPer
  :: Positive
  -- ^ Number of leading zeroes before first grouping character
  -> ZeroGroupRadPer Char ()
  -> Seq (ZeroGroupRadPer Char ())
  -> NilGroupedRadPer Char ()
cNilGroupedRadPer lead g1 gs = NilGroupedRadPer mz0 r1 z2 z3 zg4
  where
    mz0 = Zero'Opt (Just cZero)
    r1 = cRadixPer
    z2 = cZero
    z3 = case Pos.prev lead of
      Nothing -> Zero'Star Seq.empty
      Just p -> cZeroes (NN.c'NonNegative'Positive p)
    zg4 = ZeroGroupRadPer'Plus $ NE.NonEmptySeq g1 gs

-- | Has a leading zero.
cNilUngroupedRadCom
  :: NonNegative
  -- ^ Number of zeroes after the decimal point
  -> NilUngroupedRadCom Char ()
cNilUngroupedRadCom nn = case NN.c'Positive'NonNegative nn of
  Nothing -> NUZeroRadCom cZero (RadixZeroesRadCom'Opt Nothing)
  Just _ -> NUZeroRadCom cZero
    (RadixZeroesRadCom'Opt (Just (cRadixZeroesRadCom nn)))

-- | Has a leading zero.
cNilUngroupedRadPer
  :: NonNegative
  -- ^ Number of zeroes after the decimal point
  -> NilUngroupedRadPer Char ()
cNilUngroupedRadPer nn = case NN.c'Positive'NonNegative nn of
  Nothing -> NUZeroRadPer cZero (RadixZeroesRadPer'Opt Nothing)
  Just _ -> NUZeroRadPer cZero
    (RadixZeroesRadPer'Opt (Just (cRadixZeroesRadPer nn)))

cRadixComDigits :: Seq (D0'9 Char ()) -> RadixComDigits Char ()
cRadixComDigits ds = RadixComDigits cRadixCom (D0'9'Star ds)

cRadixPerDigits :: Seq (D0'9 Char ()) -> RadixPerDigits Char ()
cRadixPerDigits ds = RadixPerDigits cRadixPer (D0'9'Star ds)

cBUGreaterThanOneRadCom
  :: D1'9 Char ()
  -- ^ First digit, to left of radix
  -> Seq (D0'9 Char ())
  -- ^ Remaining digits, to left of radix
  -> Seq (D0'9 Char ())
  -- ^ Remaining digits, to right of radix
  -> BrimUngroupedRadCom Char ()
cBUGreaterThanOneRadCom d1 ds rs = BUGreaterThanOneRadCom d1 (D0'9'Star ds)
  (RadixComDigits'Opt may)
  where
    may
      | Seq.null rs = Nothing
      | otherwise = Just (cRadixComDigits rs)

cBUGreaterThanOneRadPer
  :: D1'9 Char ()
  -- ^ First digit, to left of radix
  -> Seq (D0'9 Char ())
  -- ^ Remaining digits, to left of radix
  -> Seq (D0'9 Char ())
  -- ^ Remaining digits, to right of radix
  -> BrimUngroupedRadPer Char ()
cBUGreaterThanOneRadPer d1 ds rs = BUGreaterThanOneRadPer d1 (D0'9'Star ds)
  (RadixPerDigits'Opt may)
  where
    may
      | Seq.null rs = Nothing
      | otherwise = Just (cRadixPerDigits rs)

cBULessThanOneRadCom
  :: NonNegative
  -- ^ Number of zeroes to right of radix
  -> D1'9 Char ()
  -- ^ First non-zero digit to right of radix
  -> Seq (D0'9 Char ())
  -- ^ Remaining digits to right of radix
  -> BrimUngroupedRadCom Char ()
cBULessThanOneRadCom zs d1 ds = BULessThanOneRadCom
  (Zero'Opt (Just cZero)) cRadixCom (cZeroes zs) d1 (D0'9'Star ds)

cBULessThanOneRadPer
  :: NonNegative
  -- ^ Number of zeroes to right of radix
  -> D1'9 Char ()
  -- ^ First non-zero digit to right of radix
  -> Seq (D0'9 Char ())
  -- ^ Remaining digits to right of radix
  -> BrimUngroupedRadPer Char ()
cBULessThanOneRadPer zs d1 ds = BULessThanOneRadPer
  (Zero'Opt (Just cZero)) cRadixPer (cZeroes zs) d1 (D0'9'Star ds)

-- # BrimGrouped, comma radix

cBG8NovemRadCom
  :: D1'9 Char ()
  -> Seq (D0'9 Char ())
  -> Seq (D0'9 Char (), Seq (D0'9 Char ()))
  -> BG8RadCom Char ()
cBG8NovemRadCom d1 ds gs = BG8NovemRadCom d1 (D0'9'Star ds) gs'
  where
    gs' = DigitGroupRadCom'Star (fmap (uncurry cDigitGroupRadCom) gs)

cBG8GroupRadCom
  :: BG7RadCom Char ()
  -> BG8RadCom Char ()
cBG8GroupRadCom b7 = BG8GroupRadCom cGrpRadCom b7

cBG7ZeroesRadCom
  :: Positive
  -> BG8RadCom Char ()
  -> BG7RadCom Char ()
cBG7ZeroesRadCom pos b8 = BG7ZeroesRadCom cZero zs b8
  where
    zs = case Pos.prev pos of
      Nothing -> Zero'Star Seq.empty
      Just nn -> cZeroes (NN.c'NonNegative'Positive nn)

cBG7NovemRadCom
  :: D1'9 Char ()
  -> Seq (D0'9 Char ())
  -> Seq (D0'9 Char (), Seq ((D0'9) Char ()))
  -> BG7RadCom Char ()
cBG7NovemRadCom d1 ds gs = BG7NovemRadCom d1 (D0'9'Star ds) gs'
  where
    gs' = DigitGroupRadCom'Star (fmap (uncurry cDigitGroupRadCom) gs)

cBG6NovemRadCom
  :: D1'9 Char ()
  -> Seq (D0'9 Char ())
  -> D0'9 Char ()
  -> Seq (D0'9 Char ())
  -> Seq (D0'9 Char (), Seq ((D0'9) Char ()))
  -> BG6RadCom Char ()
cBG6NovemRadCom d1 ds2 d3 ds4 gs5 = BG6NovemRadCom d1 (D0'9'Star ds2)
  cGrpRadCom d3 (D0'9'Star ds4)
  (DigitGroupRadCom'Star (fmap (uncurry cDigitGroupRadCom) gs5))

cBG6GroupRadCom
  :: BG7RadCom Char ()
  -> BG6RadCom Char ()
cBG6GroupRadCom b7 = BG6GroupRadCom cGrpRadCom b7

cBG5NovemRadCom
  :: D1'9 Char ()
  -> Seq (D0'9 Char ())
  -> D0'9 Char ()
  -> Seq (D0'9 Char ())
  -> Seq (D0'9 Char (), Seq (D0'9 Char ()))
  -> BG5RadCom Char ()
cBG5NovemRadCom d0 ds1 d2 ds3 gs4
  = BG5NovemRadCom d0 (D0'9'Star ds1) cGrpRadCom d2 (D0'9'Star ds3)
    (DigitGroupRadCom'Star (fmap (uncurry cDigitGroupRadCom) gs4))

cBG5ZeroRadCom
  :: Positive
  -- ^ Number of leading zeroes
  -> BG6RadCom Char ()
  -> BG5RadCom Char ()
cBG5ZeroRadCom pos b6 = BG5ZeroRadCom cZero rest b6
  where
    rest = case Pos.prev pos of
      Nothing -> cZeroes NN.zero
      Just pos' -> cZeroes (NN.c'NonNegative'Positive pos')

cBG4DigitRadCom
  :: D0'9 Char ()
  -> Seq (D0'9 Char ())
  -> Seq (D0'9 Char (), Seq (D0'9 Char ()))
  -> BG4RadCom Char ()
cBG4DigitRadCom d1 ds2 dss
  = BG4DigitRadCom d1 (D0'9'Star ds2)
    (DigitGroupRadCom'Star (fmap (uncurry cDigitGroupRadCom) dss))

cBG4NilRadCom :: BG4RadCom Char ()
cBG4NilRadCom = BG4NilRadCom

cBG3RadixRadCom :: BG4RadCom Char () -> BG3RadCom Char ()
cBG3RadixRadCom b4 = BG3RadixRadCom cRadixCom b4

cBG3NilRadCom :: BG3RadCom Char ()
cBG3NilRadCom = BG3NilRadCom

cBG1GroupOnLeftRadCom
  :: D0'9 Char ()
  -> Seq (D0'9 Char ())
  -> Seq (D0'9 Char (), Seq (D0'9 Char ()))
  -> BG3RadCom Char ()
  -> BG1RadCom Char ()
cBG1GroupOnLeftRadCom d0 ds1 gs2 b3 = BG1GroupOnLeftRadCom cGrpRadCom d0
  (D0'9'Star ds1)
  (DigitGroupRadCom'Star (fmap (uncurry cDigitGroupRadCom) gs2))
  b3

cBG1GroupOnRightRadCom
  :: D0'9 Char ()
  -> Seq (D0'9 Char ())
  -> D0'9 Char ()
  -> Seq (D0'9 Char ())
  -> Seq (D0'9 Char (), Seq (D0'9 Char ()))
  -> BG1RadCom Char ()
cBG1GroupOnRightRadCom d1 ds2 d4 ds5 g6
  = BG1GroupOnRightRadCom cRadixCom d1 (D0'9'Star ds2) cGrpRadCom
  d4 (D0'9'Star ds5)
  (DigitGroupRadCom'Star (fmap (uncurry cDigitGroupRadCom) g6))

cBGGreaterThanOneRadCom
  :: D1'9 Char ()
  -> Seq (D0'9 Char ())
  -> BG1RadCom Char ()
  -> BrimGroupedRadCom Char ()
cBGGreaterThanOneRadCom d0 ds1 b1
  = BGGreaterThanOneRadCom d0 (D0'9'Star ds1) b1

cBGLessThanOneRadCom
  :: BG5RadCom Char ()
  -> BrimGroupedRadCom Char ()
cBGLessThanOneRadCom b5
  = BGLessThanOneRadCom (Zero'Opt (Just cZero)) cRadixCom b5

-- # BrimGrouped, period radix

cBG8NovemRadPer
  :: D1'9 Char ()
  -> Seq (D0'9 Char ())
  -> Seq (D0'9 Char (), Seq (D0'9 Char ()))
  -> BG8RadPer Char ()
cBG8NovemRadPer d1 ds gs = BG8NovemRadPer d1 (D0'9'Star ds) gs'
  where
    gs' = DigitGroupRadPer'Star (fmap (uncurry cDigitGroupRadPer) gs)

cBG8GroupRadPer
  :: BG7RadPer Char ()
  -> BG8RadPer Char ()
cBG8GroupRadPer b7 = BG8GroupRadPer cGrpRadPer b7

cBG7ZeroesRadPer
  :: Positive
  -> BG8RadPer Char ()
  -> BG7RadPer Char ()
cBG7ZeroesRadPer pos b8 = BG7ZeroesRadPer cZero zs b8
  where
    zs = case Pos.prev pos of
      Nothing -> Zero'Star Seq.empty
      Just nn -> cZeroes (NN.c'NonNegative'Positive nn)

cBG7NovemRadPer
  :: D1'9 Char ()
  -> Seq (D0'9 Char ())
  -> Seq (D0'9 Char (), Seq (D0'9 Char ()))
  -> BG7RadPer Char ()
cBG7NovemRadPer d1 ds gs = BG7NovemRadPer d1 (D0'9'Star ds) gs'
  where
    gs' = DigitGroupRadPer'Star (fmap (uncurry cDigitGroupRadPer) gs)

cBG6NovemRadPer
  :: D1'9 Char ()
  -> Seq (D0'9 Char ())
  -> D0'9 Char ()
  -> Seq (D0'9 Char ())
  -> Seq (D0'9 Char (), Seq (D0'9 Char ()))
  -> BG6RadPer Char ()
cBG6NovemRadPer d1 ds2 d3 ds4 gs5 = BG6NovemRadPer d1 (D0'9'Star ds2)
  cGrpRadPer d3 (D0'9'Star ds4)
  (DigitGroupRadPer'Star (fmap (uncurry cDigitGroupRadPer) gs5))

cBG6GroupRadPer
  :: BG7RadPer Char ()
  -> BG6RadPer Char ()
cBG6GroupRadPer b7 = BG6GroupRadPer cGrpRadPer b7

cBG5NovemRadPer
  :: D1'9 Char ()
  -> Seq (D0'9 Char ())
  -> D0'9 Char ()
  -> Seq (D0'9 Char ())
  -> Seq (D0'9 Char (), Seq (D0'9 Char ()))
  -> BG5RadPer Char ()
cBG5NovemRadPer d0 ds1 d2 ds3 gs4
  = BG5NovemRadPer d0 (D0'9'Star ds1) cGrpRadPer d2 (D0'9'Star ds3)
    (DigitGroupRadPer'Star (fmap (uncurry cDigitGroupRadPer) gs4))

cBG5ZeroRadPer
  :: Positive
  -- ^ Number of leading zeroes
  -> BG6RadPer Char ()
  -> BG5RadPer Char ()
cBG5ZeroRadPer pos b6 = BG5ZeroRadPer cZero rest b6
  where
    rest = case Pos.prev pos of
      Nothing -> cZeroes NN.zero
      Just pos' -> cZeroes (NN.c'NonNegative'Positive pos')

cBG4DigitRadPer
  :: D0'9 Char ()
  -> Seq (D0'9 Char ())
  -> Seq (D0'9 Char (), Seq (D0'9 Char ()))
  -> BG4RadPer Char ()
cBG4DigitRadPer d1 ds2 dss
  = BG4DigitRadPer d1 (D0'9'Star ds2)
    (DigitGroupRadPer'Star (fmap (uncurry cDigitGroupRadPer) dss))

cBG4NilRadPer :: BG4RadPer Char ()
cBG4NilRadPer = BG4NilRadPer

cBG3RadixRadPer :: BG4RadPer Char () -> BG3RadPer Char ()
cBG3RadixRadPer b4 = BG3RadixRadPer cRadixPer b4

cBG3NilRadPer :: BG3RadPer Char ()
cBG3NilRadPer = BG3NilRadPer

cBG1GroupOnLeftRadPer
  :: D0'9 Char ()
  -> Seq (D0'9 Char ())
  -> Seq (D0'9 Char (), Seq (D0'9 Char ()))
  -> BG3RadPer Char ()
  -> BG1RadPer Char ()
cBG1GroupOnLeftRadPer d0 ds1 gs2 b3 = BG1GroupOnLeftRadPer cGrpRadPer d0
  (D0'9'Star ds1)
  (DigitGroupRadPer'Star (fmap (uncurry cDigitGroupRadPer) gs2))
  b3

cBG1GroupOnRightRadPer
  :: D0'9 Char ()
  -> Seq (D0'9 Char ())
  -> D0'9 Char ()
  -> Seq (D0'9 Char ())
  -> Seq (D0'9 Char (), Seq (D0'9 Char ()))
  -> BG1RadPer Char ()
cBG1GroupOnRightRadPer d1 ds2 d4 ds5 g6
  = BG1GroupOnRightRadPer cRadixPer d1 (D0'9'Star ds2) cGrpRadPer
  d4 (D0'9'Star ds5)
  (DigitGroupRadPer'Star (fmap (uncurry cDigitGroupRadPer) g6))

cBGGreaterThanOneRadPer
  :: D1'9 Char ()
  -> Seq (D0'9 Char ())
  -> BG1RadPer Char ()
  -> BrimGroupedRadPer Char ()
cBGGreaterThanOneRadPer d0 ds1 b1
  = BGGreaterThanOneRadPer d0 (D0'9'Star ds1) b1

cBGLessThanOneRadPer
  :: BG5RadPer Char ()
  -> BrimGroupedRadPer Char ()
cBGLessThanOneRadPer b5
  = BGLessThanOneRadPer (Zero'Opt (Just cZero)) cRadixPer b5

-- # Dates. Use a hyphen as separator.

cDateSep :: DateSep Char ()
cDateSep = DateSep'Hyphen cHyphen

cJan :: Days31 Char () -> MonthDay Char ()
cJan d = Jan cZero cOne cDateSep d

cFeb :: Days28 Char () -> MonthDay Char ()
cFeb d = Feb cZero cTwo cDateSep d

cMar :: Days31 Char () -> MonthDay Char ()
cMar d = Mar cZero cThree cDateSep d

cApr :: Days30 Char () -> MonthDay Char ()
cApr d = Apr cZero cFour cDateSep d

cMay :: Days31 Char () -> MonthDay Char ()
cMay d = May cZero cFive cDateSep d

cJun :: Days30 Char () -> MonthDay Char ()
cJun d = Jun cZero cSix cDateSep d

cJul :: Days31 Char () -> MonthDay Char ()
cJul d = Jul cZero cSeven cDateSep d

cAug :: Days31 Char () -> MonthDay Char ()
cAug d = Aug cZero cEight cDateSep d

cSep :: Days30 Char () -> MonthDay Char ()
cSep d = Sep cZero cNine cDateSep d

cOct :: Days31 Char () -> MonthDay Char ()
cOct d = Oct cOne cZero cDateSep d

cNov :: Days30 Char () -> MonthDay Char ()
cNov d = Nov cOne cOne cDateSep d

cDec :: Days31 Char () -> MonthDay Char ()
cDec d = Dec cOne cTwo cDateSep d

cMonthDay
  :: Int
  -- ^ Month
  -> Int
  -- ^ Day
  -> Maybe (MonthDay Char ())
cMonthDay m d
  | m == 1 = fmap cJan (c'Days31'Int d)
  | m == 2 = fmap cFeb (c'Days28'Int d)
  | m == 3 = fmap cMar (c'Days31'Int d)
  | m == 4 = fmap cApr (c'Days30'Int d)
  | m == 5 = fmap cMay (c'Days31'Int d)
  | m == 6 = fmap cJun (c'Days30'Int d)
  | m == 7 = fmap cJul (c'Days31'Int d)
  | m == 8 = fmap cAug (c'Days31'Int d)
  | m == 9 = fmap cSep (c'Days30'Int d)
  | m == 10 = fmap cOct (c'Days31'Int d)
  | m == 11 = fmap cNov (c'Days30'Int d)
  | m == 12 = fmap cDec (c'Days31'Int d)
  | otherwise = Nothing

cNonLeapDay
  :: Time.Day
  -> Maybe (NonLeapDay Char ())
cNonLeapDay timeDay = do
  let (yrI, moI, dyI) = Time.toGregorian timeDay
  yr <- c'Year'Int yrI
  moDy <- cMonthDay moI dyI
  return $ NonLeapDay yr cDateSep moDy

cLeapDay
  :: Time.Day
  -> Maybe (LeapDay Char ())
cLeapDay timeDay
  | moI == 2 && dyI == 29 = do
      ly <- c'LeapYear'Int yrI
      return $ LeapDay ly cDateSep cZero cTwo cDateSep cTwo cNine
  | otherwise = Nothing
  where
    (yrI, moI, dyI) = Time.toGregorian timeDay

cDay
  :: Time.Day
  -> Maybe (Date Char ())
cDay dy = fmap Date'NonLeapDay (cNonLeapDay dy)
  <|> fmap Date'LeapDay (cLeapDay dy)

cTimeOfDay :: Time.TimeOfDay -> Maybe (Time Char ())
cTimeOfDay (Time.TimeOfDay hrI miI secPico) = do
  let secI = round secPico
  sec <- c'Seconds'Int secI
  min <- c'Minutes'Int miI
  hr <- c'Hours'Int hrI
  return $ Time hr cColon min (ColonSeconds'Opt (Just (ColonSeconds
    cColon sec)))

cTimeZone :: Time.TimeZone -> Maybe (Zone Char ())
cTimeZone tz = do
  let signedMins = Time.timeZoneMinutes tz
      pm | signedMins < 0 = PluMin'Minus cMinus
         | otherwise = PluMin'Plus cPlus
      (hrs, mins) = abs signedMins `divMod` 60
  copperHours <- c'Hours'Int hrs
  copperMins <- c'Minutes'Int mins
  return $ Zone pm copperHours cColon copperMins

cZonedTime :: Time.ZonedTime -> Maybe (DateTimeZone Char ())
cZonedTime (Time.ZonedTime (Time.LocalTime day tod) tz) = do
  copperDay <- cDay day
  copperTime <- cTimeOfDay tod
  copperZone <- cTimeZone tz
  return $ DateTimeZone copperDay (TimeAndMayZone'Opt (Just
    (TimeAndMayZone (WhitesTime cWhite'Plus copperTime)
      (WhitesZone'Opt (Just (WhitesZone cWhite'Star copperZone))))))

space :: White'Star Char ()
space = White'Star . Seq.singleton
  $ White'Space cSpace

cWhite'Plus :: White'Plus Char ()
cWhite'Plus = White'Plus (NE.NonEmptySeq cWhite'Space Seq.empty)

newline :: White'Star Char ()
newline = White'Star . Seq.singleton . White'Newline $ cNewline

noSpace :: White'Star Char ()
noSpace = White'Star Seq.empty

-- | Makes an unquoted scalar if possible; otherwise, makes a quoted
-- scalar.
cAnyString :: Text -> AnyString Char ()
cAnyString txt = case cString . Seq.fromList . X.unpack $ txt of
  Left us -> AnyString'UnquotedString us
  Right qs -> AnyString'QuotedString qs

cDebit :: Debit Char ()
cDebit = Debit cLessThan

cCredit :: Credit Char ()
cCredit = Credit cGreaterThan

cDebitCredit :: Pole -> DebitCredit Char ()
cDebitCredit p
  | p == debit = DebitCredit'Debit cDebit
  | otherwise = DebitCredit'Credit cCredit


-- | Converts an 'A.Amount' to a 'Troika'.  A 'QC' is always created,
-- with the commodity being on the right with a space between.  The
-- decimal is always represented with a period radix and no grouping.
c'Troika'Amount :: Amount.Amount -> Troika.Troika
c'Troika'Amount (Amount.Amount cy q) = Troika.Troika cy (Troika.QC rar ar)
  where
    ar = Arrangement CommodityOnRight True
    rar = repDecimal (Right Nothing) q

cSpace'Opt :: Bool -> Space'Opt Char ()
cSpace'Opt False = Space'Opt Nothing
cSpace'Opt True = Space'Opt (Just cSpace)

cSemanticSpace :: Bool -> SemanticSpace Char ()
cSemanticSpace = SemanticSpace . cSpace'Opt

cTroiload
  :: Commodity.Commodity
  -> Troika.Troiload
  -> Maybe (Trio Char ())
cTroiload cy t = case t of

  Troika.QC (Left (Moderate nilRadCom)) (Arrangement CommodityOnLeft spc)
    -> Just $ Trio'T_Commodity_Neutral (T_Commodity_Neutral (cCommodity cy)
        (cSemanticSpace spc) neu)
    where
      neu = NeuCom cBacktick nilRadCom

  Troika.QC (Left (Moderate nilRadCom)) (Arrangement CommodityOnRight spc)
    -> Just $ Trio'T_Neutral_Commodity (T_Neutral_Commodity neu
        (cSemanticSpace spc) (cCommodity cy))
    where
      neu = NeuCom cBacktick nilRadCom

  Troika.QC (Left (Extreme (Polarized brimRadCom pole)))
    (Arrangement CommodityOnLeft spc) ->
    Just $ Trio'T_DebitCredit_Commodity_NonNeutral
    (T_DebitCredit_Commodity_NonNeutral (cDebitCredit pole) mempty
      (cCommodity cy) (cSemanticSpace spc) brim)
    where
      brim = NonNeutralRadCom cBacktick brimRadCom

  Troika.QC (Left (Extreme (Polarized brimRadCom pole)))
    (Arrangement CommodityOnRight spc) -> Just $
    Trio'T_DebitCredit_NonNeutral_Commodity
    (T_DebitCredit_NonNeutral_Commodity (cDebitCredit pole) mempty
      brim (cSemanticSpace spc) (cCommodity cy))
    where
      brim = NonNeutralRadCom cBacktick brimRadCom

  Troika.QC (Right (Moderate nilRadPer)) (Arrangement CommodityOnLeft spc)
    -> Just $ Trio'T_Commodity_Neutral (T_Commodity_Neutral (cCommodity cy)
        (cSemanticSpace spc) neu)
    where
      neu = NeuPer nilRadPer

  Troika.QC (Right (Moderate nilRadPer)) (Arrangement CommodityOnRight spc)
    -> Just $ Trio'T_Neutral_Commodity (T_Neutral_Commodity neu
        (cSemanticSpace spc) (cCommodity cy))
    where
      neu = NeuPer nilRadPer

  Troika.QC (Right (Extreme (Polarized brimRadPer pole)))
    (Arrangement CommodityOnLeft spc) ->
    Just $ Trio'T_DebitCredit_Commodity_NonNeutral
    (T_DebitCredit_Commodity_NonNeutral (cDebitCredit pole) mempty
      (cCommodity cy) (cSemanticSpace spc) brim)
    where
      brim = NonNeutralRadPer brimRadPer

  Troika.QC (Right (Extreme (Polarized brimRadPer pole)))
    (Arrangement CommodityOnRight spc) ->
    Just $ Trio'T_DebitCredit_NonNeutral_Commodity
    (T_DebitCredit_NonNeutral_Commodity (cDebitCredit pole) mempty
      brim (cSemanticSpace spc) (cCommodity cy))
    where
      brim = NonNeutralRadPer brimRadPer

  Troika.Q (Left (Moderate nilRadCom)) -> Just $ Trio'T_Neutral (T_Neutral neu)
    where
      neu = NeuCom cBacktick nilRadCom

  Troika.Q (Right (Moderate nilRadPer)) -> Just $ Trio'T_Neutral (T_Neutral neu)
    where
      neu = NeuPer nilRadPer

  Troika.Q (Left (Extreme (Polarized brimRadCom pole))) ->
    Just $ Trio'T_DebitCredit_NonNeutral (T_DebitCredit_NonNeutral
    (cDebitCredit pole) mempty nn)
    where
      nn = NonNeutralRadCom cBacktick brimRadCom

  Troika.Q (Right (Extreme (Polarized brimRadPer pole))) ->
    Just $ Trio'T_DebitCredit_NonNeutral (T_DebitCredit_NonNeutral
    (cDebitCredit pole) mempty nn)
    where
      nn = NonNeutralRadPer brimRadPer

  Troika.SC dnz -> Just $ Trio'T_DebitCredit_Commodity
    (T_DebitCredit_Commodity (cDebitCredit pole) mempty (cCommodity cy))
    where
      pole = Lens.view poleDecNonZero dnz

  Troika.S dnz -> Just $ Trio'T_DebitCredit
    . T_DebitCredit . cDebitCredit .  Lens.view poleDecNonZero $ dnz

  Troika.UC brim _ (Arrangement CommodityOnLeft spc) ->
    Just $ Trio'T_Commodity_NonNeutral (T_Commodity_NonNeutral (cCommodity cy)
    (cSemanticSpace spc) nn)
    where
      nn = case brim of
        Left brc -> NonNeutralRadCom cBacktick brc
        Right brp -> NonNeutralRadPer brp

  Troika.UC brim _ (Arrangement CommodityOnRight spc) ->
    Just $ Trio'T_NonNeutral_Commodity (T_NonNeutral_Commodity nn
    (cSemanticSpace spc) (cCommodity cy))
    where
      nn = case brim of
        Left brc -> NonNeutralRadCom cBacktick brc
        Right brp -> NonNeutralRadPer brp

  Troika.NC nil (Arrangement CommodityOnLeft spc) ->
    Just $ Trio'T_Commodity_Neutral (T_Commodity_Neutral (cCommodity cy)
    (cSemanticSpace spc) nar)
    where
      nar = case nil of
        Left nrc -> NeuCom cBacktick nrc
        Right nrp -> NeuPer nrp

  Troika.NC nil (Arrangement CommodityOnRight spc) ->
    Just $ Trio'T_Neutral_Commodity (T_Neutral_Commodity nar
    (cSemanticSpace spc) (cCommodity cy))
    where
      nar = case nil of
        Left nrc -> NeuCom cBacktick nrc
        Right nrp -> NeuPer nrp

  Troika.US brim _ -> Just $ Trio'T_NonNeutral (T_NonNeutral b)
    where
      b = case brim of
        Left brc -> NonNeutralRadCom cBacktick brc
        Right brp -> NonNeutralRadPer brp

  Troika.UU nar -> Just $ Trio'T_Neutral (T_Neutral n)
    where
      n = case nar of
        Left nrc -> NeuCom cBacktick nrc
        Right nrp -> NeuPer nrp

  Troika.C _ -> Just $ Trio'T_Commodity (T_Commodity (cCommodity cy))

  Troika.E _ -> Nothing

-- | Succeeds if the 'Troika.Troiquant' is anything other than
-- 'Troika.E'; otherwise, returns Nothing.
cTroika
  :: Troika.Troika
  -> Maybe (Trio Char ())
cTroika (Troika.Troika cy tl) = cTroiload cy tl

-- | Amounts are always frozen as an ungrouped representation with
-- the commodity on the left with no space between.
cAmount
  :: Amount.Amount
  -> Trio Char ()
cAmount (Amount.Amount cy q) = case splitRepAnyRadix rep of
  Left nil -> Trio'T_Commodity_Neutral tComNeu
    where
      tComNeu = T_Commodity_Neutral cy' (cSemanticSpace False) neu
      neu = case nil of
        Left nilCom -> NeuCom cBacktick nilCom
        Right nilPer -> NeuPer nilPer
  Right (brim, pole) -> Trio'T_DebitCredit_Commodity_NonNeutral tDrComNon
    where
      tDrComNon = T_DebitCredit_Commodity_NonNeutral drCr mempty cy'
        (cSemanticSpace False) nn
        where
          drCr | pole == debit = DebitCredit'Debit (Debit cLessThan)
               | otherwise = DebitCredit'Credit (Credit cGreaterThan)
          nn = case brim of
            Left brimCom -> NonNeutralRadCom cBacktick brimCom
            Right brimPer -> NonNeutralRadPer brimPer
  where
    rep = repDecimal (Right Nothing) q
    cy' = cCommodity cy

-- * Prices

cExch :: Decimal -> Exch Char ()
cExch dec = case splitRepAnyRadix . repDecimal (Right Nothing) $ dec of
  Left nil -> Exch'Neutral $ case nil of
    Left nilCom -> NeuCom cBacktick nilCom
    Right nilPer -> NeuPer nilPer
  Right (brim, pole) -> Exch'ExchNonNeu $ ExchNonNeu'PluMinNonNeutral
    (PluMinNonNeutral pm mempty nonNeu)
    where
      pm | pole == negative = PluMin'Minus cMinus
          | otherwise = PluMin'Plus cPlus
      nonNeu = case brim of
        Left brimCom -> NonNeutralRadCom cBacktick brimCom
        Right brimPer -> NonNeutralRadPer brimPer

cPrice
  :: PriceParts a
  -> Maybe (Price Char ())
cPrice (PriceParts _ zt from to dec) = fmap f (cZonedTime zt)
  where
    f dtz = Price cAtSign cWhite'Star dtz cWhite'Plus
      (cCommodity from) cWhite'Plus jan cNewline
    jan = Janus'CyExch $ CyExch (cCommodity to) cWhite'Star
      (cExch dec)

-- Labels

cSeries :: String -> NonEmptySeq (Char, ())
cSeries = fromJust . NE.seqToNonEmptySeq . Seq.fromList
  . fmap (\c -> (c, ()))

cLblOrigPayee :: LblOrigPayee Char ()
cLblOrigPayee = LblOrigPayee $ cSeries "'origPayee'"

cLblNumber :: LblNumber Char ()
cLblNumber = LblNumber $ cSeries "'number'"

cLblFitid :: LblFitid Char ()
cLblFitid = LblFitid $ cSeries "'fitid'"

cLblTags :: LblTags Char ()
cLblTags = LblTags $ cSeries "'tags'"

cLblMemo :: LblMemo Char ()
cLblMemo = LblMemo $ cSeries "'memo'"

cLblUid :: LblUid Char ()
cLblUid = LblUid $ cSeries "'uid'"

cLblOfxTrn :: LblOfxTrn Char ()
cLblOfxTrn = LblOfxTrn $ cSeries "'ofxtrn'"

cLblOrigDate :: LblOrigDate Char ()
cLblOrigDate = LblOrigDate $ cSeries "'origDate'"

cNextListItem :: Text -> NextListItem Char ()
cNextListItem = NextListItem cWhite'Plus . cAnyString

cNextListItem'Star :: Seq Text -> NextListItem'Star Char ()
cNextListItem'Star = NextListItem'Star . fmap cNextListItem

cListItems :: NonEmptySeq Text -> ListItems Char ()
cListItems (NE.NonEmptySeq t1 ts) = ListItems cWhite'Star (cAnyString t1)
  (cNextListItem'Star ts)

cListItems'Opt :: Seq Text -> ListItems'Opt Char ()
cListItems'Opt = ListItems'Opt . maybe Nothing (Just . cListItems)
  . NE.seqToNonEmptySeq

cBracketedList :: Seq Text -> BracketedList Char ()
cBracketedList ts = BracketedList cOpenSquare (cListItems'Opt ts)
  cWhite'Star cCloseSquare

cTCREDIT :: TCREDIT Char ()
cTCREDIT = TCREDIT $ cSeries "TCREDIT"

cTDEBIT :: TDEBIT Char ()
cTDEBIT = TDEBIT $ cSeries "TDEBIT"

cTINT :: TINT Char ()
cTINT = TINT $ cSeries "TINT"

cTDIV :: TDIV Char ()
cTDIV = TDIV $ cSeries "TDIV"

cTFEE :: TFEE Char ()
cTFEE = TFEE $ cSeries "TFEE"

cTSRVCHG :: TSRVCHG Char ()
cTSRVCHG = TSRVCHG $ cSeries "TSRVCHG"

cTDEP :: TDEP Char ()
cTDEP = TDEP $ cSeries "TDEP"

cTATM :: TATM Char ()
cTATM = TATM $ cSeries "TATM"

cTPOS :: TPOS Char ()
cTPOS = TPOS $ cSeries "TPOS"

cTXFER :: TXFER Char ()
cTXFER = TXFER $ cSeries "TXFER"

cTCHECK :: TCHECK Char ()
cTCHECK = TCHECK $ cSeries "TCHECK"

cTPAYMENT :: TPAYMENT Char ()
cTPAYMENT = TPAYMENT $ cSeries "TPAYMENT"

cTCASH :: TCASH Char ()
cTCASH = TCASH $ cSeries "TCASH"

cTDIRECTDEP :: TDIRECTDEP Char ()
cTDIRECTDEP = TDIRECTDEP $ cSeries "TDIRECTDEP"

cTDIRECTDEBIT :: TDIRECTDEBIT Char ()
cTDIRECTDEBIT = TDIRECTDEBIT $ cSeries "TDIRECTDEBIT"

cTREPEATPMT :: TREPEATPMT Char ()
cTREPEATPMT = TREPEATPMT $ cSeries "TREPEATPMT"

cTOTHER :: TOTHER Char ()
cTOTHER = TOTHER $ cSeries "TOTHER"

cTrnType :: OFX.TrnType -> OfxTrnData Char ()
cTrnType x = case x of
  OFX.TCREDIT -> OfxTrnData'TCREDIT cTCREDIT
  OFX.TDEBIT -> OfxTrnData'TDEBIT cTDEBIT
  OFX.TINT -> OfxTrnData'TINT cTINT
  OFX.TDIV -> OfxTrnData'TDIV cTDIV
  OFX.TFEE -> OfxTrnData'TFEE cTFEE
  OFX.TSRVCHG -> OfxTrnData'TSRVCHG cTSRVCHG
  OFX.TDEP -> OfxTrnData'TDEP cTDEP
  OFX.TATM -> OfxTrnData'TATM cTATM
  OFX.TPOS -> OfxTrnData'TPOS cTPOS
  OFX.TXFER -> OfxTrnData'TXFER cTXFER
  OFX.TCHECK -> OfxTrnData'TCHECK cTCHECK
  OFX.TPAYMENT -> OfxTrnData'TPAYMENT cTPAYMENT
  OFX.TCASH -> OfxTrnData'TCASH cTCASH
  OFX.TDIRECTDEP -> OfxTrnData'TDIRECTDEP cTDIRECTDEP
  OFX.TDIRECTDEBIT -> OfxTrnData'TDIRECTDEBIT cTDIRECTDEBIT
  OFX.TREPEATPMT -> OfxTrnData'TREPEATPMT cTREPEATPMT
  OFX.TOTHER -> OfxTrnData'TOTHER cTOTHER

cDateField :: Time.ZonedTime -> Maybe (DateField Char ())
cDateField = fmap DateField . cZonedTime

cPayee :: Text -> Payee Char ()
cPayee = Payee . cAnyString

cOrigPayee :: Text -> OrigPayee Char ()
cOrigPayee txt = OrigPayee cWhite'Star cLblOrigPayee cWhite'Star
  (cAnyString txt)

cNumber :: Integer -> Number Char ()
cNumber = Number cLblNumber cWhite'Star . cInteger

cFlag :: Text -> Flag Char ()
cFlag txt = Flag cOpenParen cWhite'Star (cAnyString txt) cWhite'Star
  cCloseParen

cAccount :: Seq Text -> Account Char ()
cAccount = Account . cBracketedList

cFitid :: Text -> Fitid Char ()
cFitid = Fitid cLblFitid cWhite'Star . cAnyString

cTags :: Seq Text -> Tags Char ()
cTags = Tags cLblTags cWhite'Star . cBracketedList

cMemo :: Seq Text -> Memo Char ()
cMemo = Memo cLblMemo cWhite'Star . cBracketedList

cUid :: Text -> Uid Char ()
cUid = Uid cLblUid cWhite'Star . cAnyString

cOfxTrn :: OFX.TrnType -> OfxTrn Char ()
cOfxTrn = OfxTrn cLblOfxTrn cWhite'Star . cTrnType

cOrigDate :: Time.ZonedTime -> Maybe (OrigDate Char ())
cOrigDate zt = OrigDate cLblOrigDate cWhite'Star <$> cZonedTime zt

cPostingFields :: Fields.PostingFields -> Maybe (PostingFields Char ())
cPostingFields (Fields.PostingFields num fl acct fitid
  tags uid trnType origDate memo)
  = fmap f copperOrigDate
  where
    copperOrigDate = case origDate of
      Nothing -> Just Nothing
      Just d -> fmap Just $ cOrigDate d
    f od = PostingFields acctField (PostingFieldP'Star (tagsField
      `Lens.cons` otherFields))
      where
        acctField = PostingField'Account (cAccount acct)
        tagsField = PostingFieldP cWhite'Plus (PostingField'Tags (cTags tags))
        otherFields
          = fmap (PostingFieldP cWhite'Plus)
          . catMaybes
          $ fmap (PostingField'Number . cNumber) num
          <| (Just . PostingField'Flag . cFlag) fl
          <| (Just . PostingField'Fitid . cFitid) fitid
          <| (Just . PostingField'Uid . cUid) uid
          <| fmap (PostingField'OfxTrn . cOfxTrn) trnType
          <| fmap PostingField'OrigDate od
          <| (Just . PostingField'Memo . cMemo) memo
          <| Seq.empty

cTopLineFields :: Fields.TopLineFields -> Maybe (TopLineFields Char ())
cTopLineFields (Fields.TopLineFields zt pye origPye)
  = fmap f (cZonedTime zt)
  where
    f dtz = TopLineFields (DateField dtz) cWhite'Plus
      (cPayee pye) (OrigPayee'Opt . Just . cOrigPayee $ origPye)

cTopLine :: Tranche.TopLine a -> Maybe (TopLineFields Char ())
cTopLine (Tranche.Tranche _ tlf) = cTopLineFields tlf

-- | Fails if the 'Fields._origDate' field contains a bad date.
cEnt :: (Troika.Troika, Tranche.Postline a) -> Maybe (Posting Char ())
cEnt (tka, Tranche.Tranche _ pf) = case cTroika tka of
  Nothing -> fmap Posting'PostingFields (cPostingFields pf)
  Just trio -> fmap f (cPostingFields pf)
    where
      f = Posting'TrioMaybeFields
        . TrioMaybeFields trio
        . PostingFieldsP'Opt
        . Just
        . PostingFieldsP mempty


spaces :: Int -> White'Star Char ()
spaces i = White'Star . fmap White'Space . Seq.replicate i
  $ cSpace


-- | An error occurred when trying to freeze a single price,
-- transaction, or comment.
data TracompriError a
  = TracompriBadTime (Txn.Transaction a)
  -- ^ Could not freeze the transaction because the 'Fields._zonedTime' was bad.
  | TracompriBadOrigDate (Txn.Transaction a)
  -- ^ Could not freeze the transaction because the 'Fields._origDate' was bad.
  | TracompriBadComment Text (NonEmptySeq Char)
  -- ^ Could not freeze a comment line due to at least one invalid
  -- character.  As many invalid characters as possible are
  -- accumulated.  The 'Text' is the entire invalid comment.
  | TracompriBadPrice (PriceParts a)
  -- ^ Could not freeze a price because the date was bad.
  -- The entire 'PriceParts' is here.
  deriving Show

tracompriComment
  :: Text
  -> Accuerr (NonEmptySeq (TracompriError a)) (Comment Char ())
tracompriComment txt
  = Lens.over Accuerr._AccFailure (NE.singleton . TracompriBadComment txt)
  . cComment
  $ txt

tracompriPrice
  :: PriceParts a
  -> Accuerr (NonEmptySeq (TracompriError a)) (Price Char ())
tracompriPrice p
  = maybe (Accuerr.AccFailure . NE.singleton . TracompriBadPrice $ p) pure
  . cPrice
  $ p

tracompriEnt
  :: Txn.Transaction a
  -- ^ Original transaction.  Used only for errors.
  -> (Troika.Troika, Tranche.Postline a)
  -> Accuerr (NonEmptySeq (TracompriError a)) (Posting Char ())
tracompriEnt txn = maybe err pure . cEnt
  where
    err = Accuerr.AccFailure . NE.singleton . TracompriBadOrigDate $ txn

tracompriTopLine
  :: Txn.Transaction a
  -- ^ Original transaction.  The postings are needed only for errors.
  -> Accuerr (NonEmptySeq (TracompriError a)) (TopLineFields Char ())
tracompriTopLine txn@(Txn.Transaction tl _)
  = maybe err pure . cTopLine $ tl
  where
    err = Accuerr.AccFailure . NE.singleton . TracompriBadTime $ txn

tracompriTransaction
  :: Txn.Transaction a
  -> Accuerr (NonEmptySeq (TracompriError a)) (Transaction Char ())
tracompriTransaction txn@(Txn.Transaction _ pstgs)
  = Transaction
  <$> tracompriTopLine txn
  <*> pure mempty
  <*> fmap mkPstgs (traverse (tracompriEnt txn) (balancedToSeqEnt pstgs))
  where
    mkPstgs pstgs' = Postings cOpenCurly mayPl mempty cCloseCurly cNewline
      where
        mayPl = PostingList'Opt . maybe Nothing mkPstgList
          . Lens.uncons $ pstgs'
          where
            mkPstgList (p1, ps) = Just . PostingList mempty p1
              . NextPosting'Star . fmap mkNextPosting $ ps
              where
                mkNextPosting = NextPosting mempty cSemicolon mempty

tracompri
  :: Tracompri a
  -> Accuerr (NonEmptySeq (TracompriError a)) (FileItem Char ())
tracompri tra = case tra of
  Tracompri'Transaction txn -> fmap FileItem'Transaction
    $ tracompriTransaction txn
  Tracompri'Comment com -> fmap FileItem'Comment
    $ tracompriComment com
  Tracompri'Price pri -> fmap FileItem'Price
    . tracompriPrice
    . priceToPriceParts $ pri

tracompriWholeFile
  :: Seq (Tracompri a)
  -> Accuerr (NonEmptySeq (TracompriError a)) (WholeFile Char ())
tracompriWholeFile sq
  = WholeFile
  <$> fmap FileItemP'Star (traverse mkFileItemP sq)
  <*> pure mempty
  where
    mkFileItemP = fmap (FileItemP mempty) . tracompri
