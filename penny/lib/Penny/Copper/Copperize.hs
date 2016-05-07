{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- | Copperize takes other data types, both from Penny and from
-- other libraries, and turns them into Copper data types.
--
-- The Earley parsers can already be thought of as copperizers for
-- strings.  Some of the functions in this module rely on that: for
-- instance 'cDay' takes a 'Time.Day', 'show's it, and then parses
-- that string.
--
-- Other functions do not use the parser but instead construct the
-- necessary data types directly.
module Penny.Copper.Copperize where

import Control.Applicative ((<|>))
import qualified Control.Lens as Lens
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as X
import qualified Pinchot
import Text.Earley (Prod, Grammar)
import qualified Text.Earley as Earley
import qualified Data.Time as Time

import Penny.Arrangement
import Penny.Amount
import qualified Penny.Commodity as Commodity
import qualified Penny.Scalar as Scalar
import qualified Penny.Tree as Tree
import Penny.Copper.EarleyGrammar
import Penny.Copper.Grouping
import Penny.Copper.Optics
import Penny.Copper.PriceParts
import Penny.Copper.Productions
import Penny.Copper.Terminalizers
import Penny.Copper.Types
import Penny.Decimal
import Penny.NonNegative (NonNegative)
import qualified Penny.NonNegative as NN
import Penny.Polar
import Penny.Positive (Positive)
import qualified Penny.Positive as Pos
import Penny.Rep
import Penny.SeqUtil


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

c'D0'2'Int :: Integral a => a -> Maybe (D0'2 Char ())
c'D0'2'Int x = case x of
  0 -> Just $ D0'2'Zero (Zero ('0', ()))
  1 -> Just $ D0'2'One (One ('1', ()))
  2 -> Just $ D0'2'Two (Two ('2', ()))
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
cOpenSquare = OpenSquare (']', ())

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

cCommentChar :: Char -> Maybe (CommentChar Char ())
cCommentChar c = Lens.preview _CommentChar (c, ())

-- | Converts a 'Text' to a 'CommentChar'Star'.  Fails if any
-- character cannot be placed into a 'CommentChar' and returns a
-- 'Left' with the bad character.
cCommentChar'Star :: Text -> Either Char (CommentChar'Star Char ())
cCommentChar'Star
  = fmap CommentChar'Star . traverse f . Seq.fromList . X.unpack
  where
    f c = case cCommentChar c of
      Nothing -> Left c
      Just r -> Right r

cComment :: Text -> Either Char (Comment Char ())
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

cWhite'Comment :: Text -> Either Char (White Char ())
cWhite'Comment txt = White'Comment <$> cComment txt

-- | Replicates the given 'White' to return a 'White'Star'.
cWhite'Star :: Int -> White Char () -> White'Star Char ()
cWhite'Star i w = White'Star (Seq.replicate i w)

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

-- | Parses a string into a value.  If there are no parsed results,
-- fails.  If there is more than one parsed result, applies 'error',
-- because that indicates an ambiguous grammar (there should be no
-- ambiguous grammars).
parse
  :: (forall r. Grammar r (Prod r String (Char, ()) (a Char ())))
  -> String
  -> Maybe (a Char ())
parse grammar str = case results of
  [] -> Nothing
  x:[] -> Just x
  _ -> error $ "parse: ambiguous grammar with string: " ++ str
  where
    (results, _) = Earley.fullParses (Earley.parser grammar)
      . Pinchot.noLocations $ str

cDay :: Time.Day -> Maybe (Date Char ())
cDay = parse (fmap a'Date earleyGrammar) . show

cTimeOfDay :: Time.TimeOfDay -> Maybe (Time Char ())
cTimeOfDay = parse (fmap a'Time earleyGrammar) . show

cTimeZone :: Time.TimeZone -> Maybe (Zone Char ())
cTimeZone = fmap (Zone cBacktick)
  . parse (fmap a'ZoneHrsMins earleyGrammar)
  . Time.timeZoneOffsetString

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
  return $ UnquotedStringNonDigitChar'Plus (Pinchot.NonEmpty x' xs')

cCommodity :: Seq Char -> Commodity Char ()
cCommodity sq = case cUnquotedStringNonDigitChar'Plus sq of
  Nothing -> Commodity'QuotedCommodity (QuotedCommodity (cQuotedString sq))
  Just us -> Commodity'UnquotedCommodity (UnquotedCommodity us)


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
    zg4 = ZeroGroupRadCom'Plus $ Pinchot.NonEmpty g1 gs

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
    zg4 = ZeroGroupRadPer'Plus $ Pinchot.NonEmpty g1 gs

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

cNonLeapDay
  :: Year Char ()
  -> MonthDay Char ()
  -> NonLeapDay Char ()
cNonLeapDay y md = NonLeapDay y cDateSep md

cLeapDay :: LeapYear Char () -> LeapDay Char ()
cLeapDay ly = LeapDay ly cDateSep cZero cTwo cDateSep cTwo cNine

cTime
  :: Hours Char ()
  -> (D0'5 Char (), D0'9 Char ())
  -- ^ Minutes
  -> Maybe (D0'5 Char (), D0'9 Char ())
  -- ^ Seconds
  -> Time Char ()
cTime h (m1, m2) s = Time h cColon (Minutes (N0'59 m1 m2)) s'
  where
    s' = case s of
      Nothing -> ColonSeconds'Opt Nothing
      Just (s1, s2) -> ColonSeconds'Opt (Just (ColonSeconds cColon
        (Seconds (N0'59 s1 s2))))

cZone
  :: Pole
  -- ^ Positive or negative
  -> D0'2 Char ()
  -> D0'3 Char ()
  -> D0'9 Char ()
  -> D0'9 Char ()
  -> Zone Char ()
cZone p d0 d1 d2 d3 = Zone cBacktick
  (ZoneHrsMins pm d0 d1 d2 d3)
  where
    pm | p == positive = PluMin'Plus cPlus
       | otherwise = PluMin'Minus cMinus

space :: White'Star Char ()
space = White'Star . Seq.singleton
  $ White'Space cSpace

newline :: White'Star Char ()
newline = White'Star . Seq.singleton . White'Newline $ cNewline

noSpace :: White'Star Char ()
noSpace = White'Star Seq.empty

cNextTree :: Tree Char () -> NextTree Char ()
cNextTree tree = NextTree noSpace cComma space tree

cNextTree'Star :: Seq (Tree Char ()) -> NextTree'Star Char ()
cNextTree'Star = NextTree'Star . fmap cNextTree

cForest :: Tree Char () -> Seq (Tree Char ()) -> Forest Char ()
cForest t1 ts = Forest t1 (cNextTree'Star ts)

cBracketedForest :: Forest Char () -> BracketedForest Char ()
cBracketedForest f
  = BracketedForest cOpenSquare space f space cCloseSquare

cTree :: Scalar Char () -> Maybe (Forest Char ()) -> Tree Char ()
cTree sc mayForest = Tree'ScalarMaybeForest
  (ScalarMaybeForest sc (WhitesBracketedForest'Opt may))
  where
    may = case mayForest of
      Nothing -> Nothing
      Just fr -> Just (WhitesBracketedForest space (cBracketedForest fr))

spinster :: Scalar Char () -> Tree Char ()
spinster s = cTree s Nothing

orphans :: Tree Char () -> Seq (Tree Char ()) -> Tree Char ()
orphans t1 ts = Tree'ForestMaybeScalar
  (ForestMaybeScalar (cBracketedForest (cForest t1 ts))
                     (WhitesScalar'Opt Nothing))

-- | Makes an unquoted scalar if possible; otherwise, makes a quoted
-- scalar.
textScalar :: Text -> Scalar Char ()
textScalar txt = case cString . Seq.fromList . X.unpack $ txt of
  Left us -> Scalar'UnquotedString us
  Right qs -> Scalar'QuotedString qs

cDateTimeZone
  :: Date Char ()
  -> Time Char ()
  -> Zone Char ()
  -> Forest Char ()
cDateTimeZone date time zone = cForest dateTree
  . Seq.fromList $ [timeTree, zoneTree]
  where
    dateTree = spinster (Scalar'Date date)
    timeTree = spinster (Scalar'Time time)
    zoneTree = spinster (Scalar'Zone zone)

