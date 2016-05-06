{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Penny.Copper.Copperize where

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

import Penny.Copper.EarleyGrammar
import Penny.Copper.Productions
import Penny.Copper.Types
import Penny.Decimal
import Penny.NonNegative (NonNegative)
import qualified Penny.NonNegative as NN
import Penny.Positive (Positive)
import qualified Penny.Positive as Pos
import Penny.Copper.Optics


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
  0 -> Just $ D0'1'Zero sZero
  1 -> Just $ D0'1'One sOne
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
      return $ D28'1to9 sZero d
    d10to19 = do
      d <- c'D0'9'Int (x - 10)
      return $ D28'10to19 sOne d
    d20to28 = do
      d <- c'D0'8'Int (x - 20)
      return $ D28'20to28 sTwo d

c'Days30'Int :: Integral a => a -> Maybe (Days30 Char ())
c'Days30'Int x = d28 <|> d29 <|> d30
  where
    d28 = fmap D30'28 (c'Days28'Int x)
    d29 | x == 29 = Just $ D30'29 sTwo sNine
        | otherwise = Nothing
    d30 | x == 30 = Just $ D30'30 sThree sZero
        | otherwise = Nothing

c'Days31'Int :: Integral a => a -> Maybe (Days31 Char ())
c'Days31'Int x = d30 <|> d31
  where
    d30 = fmap D31'30 $ c'Days30'Int x
    d31 | x == 31 = Just $ D31'31 sThree sOne
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
  { 4 -> Just $ L04 sZero sFour; 8 -> Just $ L08 sZero sEight;
    12 -> Just $ L12 sOne sTwo;
    16 -> Just $ L16 sOne sSix; 20 -> Just $ L20 sTwo sZero;
    24 -> Just $ L24 sTwo sFour;
    28 -> Just $ L28 sTwo sEight; 32 -> Just $ L32 sThree sTwo;
    36 -> Just $ L36 sThree sSix;
    40 -> Just $ L40 sFour sZero; 44 -> Just $ L44 sFour sFour;
    48 -> Just $ L48 sFour sEight;
    52 -> Just $ L52 sFive sTwo; 56 -> Just $ L56 sFive sSix;
    60 -> Just $ L60 sSix sZero;
    64 -> Just $ L64 sSix sFour; 68 -> Just $ L68 sSix sEight;
    72 -> Just $ L72 sSeven sTwo;
    76 -> Just $ L76 sSeven sSix; 80 -> Just $ L80 sEight sZero;
    84 -> Just $ L84 sEight sFour;
    88 -> Just $ L88 sEight sEight; 92 -> Just $ L92 sNine sTwo;
    96 -> Just $ L96 sNine sSix;
    _ -> Nothing }

c'CenturyLeapYear'Int :: Integral a => a -> Maybe (CenturyLeapYear Char ())
c'CenturyLeapYear'Int x
  | x == 0 = Just $ LeapYear0 sZero sZero sZero sZero
  | rm == 0 = do
      m4 <- c'Mod4'Int dv
      return $ LeapYearMod4 m4 sZero sZero
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
      return (N0'19 (D0'1'Opt (Just (D0'1'One sOne))) d1)
    d0'9 = do
      d0'9 <- c'D0'9'Int x
      return $ N0'19 (D0'1'Opt Nothing) d0'9

c'N20'23'Int :: Integral a => a -> Maybe (N20'23 Char ())
c'N20'23'Int x = do
  d1 <- c'D0'3'Int $ x - 20
  return $ N20'23 sTwo d1

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

cDateSep :: DateSep Char ()
cDateSep = DateSep'Hyphen cHyphen

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
  = case diff (next $ length dr) expt of
      Equal -> BULessThanOneRadCom (Zero'Opt $ Just sZero)
        rdx (Zero'Star S.empty) d1 (D0'9'Star dr)
      LeftBiggerBy l -> BUGreaterThanOneRadCom d1 (D0'9'Star leftDigs)
        (RadixComDigits'Opt rightDigs)
        where
          (leftDigs, rightDigs) = case Pos.prev l of
            Nothing -> (S.empty, Just (RadixComDigits rdx (D0'9'Star dr)))
            Just c -> (beg,
              Just (RadixComDigits rdx (D0'9'Star end)))
              where
                (beg, end) = S.splitAt (integerToInt $ Pos.c'Integer'Positive c) dr
      RightBiggerBy r -> BULessThanOneRadCom (Zero'Opt $ Just sZero)
        rdx (Zero'Star zs) d1 (D0'9'Star dr)
        where
          zs = flip S.replicate sZero
            . integerToInt . Pos.c'Integer'Positive $ r
  where
    rdx = sRadixCom

repDigitsRadPer
  :: (D1'9 Char (), Seq (D0'9 Char ()))
  -- ^ Significand
  -> NonNegative
  -- ^ Exponent
  -> BrimUngroupedRadPer Char () 
repDigitsRadPer (d1, dr) expt
  = case diff (next $ length dr) expt of
      Equal -> BULessThanOneRadPer (Zero'Opt $ Just sZero)
        rdx (Zero'Star S.empty) d1 (D0'9'Star dr)
      LeftBiggerBy l -> BUGreaterThanOneRadPer d1 (D0'9'Star leftDigs)
        (RadixPerDigits'Opt rightDigs)
        where
          (leftDigs, rightDigs) = case Pos.prev l of
            Nothing -> (S.empty, Just (RadixPerDigits rdx (D0'9'Star dr)))
            Just c -> (beg,
              Just (RadixPerDigits rdx (D0'9'Star end)))
              where
                (beg, end) = S.splitAt (integerToInt $ Pos.c'Integer'Positive c) dr
      RightBiggerBy r -> BULessThanOneRadPer (Zero'Opt $ Just sZero)
        rdx (Zero'Star zs) d1 (D0'9'Star dr)
        where
          zs = flip S.replicate sZero
            . integerToInt . Pos.c'Integer'Positive $ r
  where
    rdx = sRadixPer

repUngroupedDecZeroRadCom
  :: DecZero
  -> NilUngroupedRadCom Char ()
repUngroupedDecZeroRadCom (Exponential () expt) = NUZeroRadCom sZero
  (RadixZeroesRadCom'Opt mayRdx)
  where
    rdx = sRadixCom
    mayRdx
      | expt == zero = Nothing
      | otherwise = Just (RadixZeroesRadCom rdx (Zero'Star zs))
      where
        zs = S.replicate (integerToInt . c'Integer'NonNegative $ expt) sZero

repUngroupedDecZeroRadPer
  :: DecZero
  -> NilUngroupedRadPer Char ()
repUngroupedDecZeroRadPer (Exponential () expt) = NUZeroRadPer sZero
  (RadixZeroesRadPer'Opt mayRdx)
  where
    rdx = sRadixPer
    mayRdx
      | expt == zero = Nothing
      | otherwise = Just (RadixZeroesRadPer rdx (Zero'Star zs))
      where
        zs = S.replicate (integerToInt . c'Integer'NonNegative $ expt) sZero

repUngroupedDecPositiveRadCom
  :: DecPositive
  -> BrimUngroupedRadCom Char ()
repUngroupedDecPositiveRadCom (Exponential sig expt)
  = repDigitsRadCom (Conv.positiveDigits sig) expt

repUngroupedDecPositiveRadPer
  :: DecPositive
  -> BrimUngroupedRadPer Char ()
repUngroupedDecPositiveRadPer (Exponential sig expt)
  = repDigitsRadPer (Conv.positiveDigits sig) expt

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
displayDecimalAsQty d = (toList (sideChar <| ' ' <| rest) ++)
  where
    sideChar = case integerPole . _coefficient $ d of
      Nothing -> ' '
      Just v
        | v == debit -> '<'
        | otherwise -> '>'
    rest = fmap fst . toList $ case repUngroupedDecimalRadPer d of
      Moderate nu -> t'NilUngroupedRadPer nu
      Extreme (Polarized bu _) -> t'BrimUngroupedRadPer bu


