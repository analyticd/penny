{-# LANGUAGE FlexibleContexts #-}
module Penny.Copper.Ast where

{-

import Control.Applicative
import Text.Megaparsec
  (SourcePos, getPosition, char, (<?>), try)
import Text.Megaparsec.Text (Parser)

import Penny.Copper.Terminals
import Penny.Copper.Types
import Penny.Copper.Date
import Penny.Digit
import Penny.DateTime
import Penny.Representation
import Penny.Polar
import Penny.PluMin


data Located a = Located SourcePos a
  deriving (Eq, Ord, Show)

instance Functor Located where
  fmap f (Located l1 a) = Located l1 (f a)

pLocated :: Parser a -> Parser (Located a)
pLocated p = Located <$> getPosition <*> p

rLocated :: (a -> ShowS) -> Located a -> ShowS
rLocated f (Located _ a) = f a

-- | Octothorpe
data Hash = Hash
  deriving (Eq, Ord, Show)

pHash :: Parser Hash
pHash = Hash <$ char '#'

rHash :: Hash -> ShowS
rHash Hash = ('#':)

data Newline = Newline
  deriving (Eq, Ord, Show)

pNewline :: Parser Newline
pNewline = Newline <$ char '\n'

rNewline :: Newline -> ShowS
rNewline Newline = ('\n':)

data Comment = Comment Hash [CommentChar] Newline
  deriving (Eq, Ord, Show)

rComment :: Comment -> ShowS
rComment (Comment h0 cs1 n2) = rHash h0 . rList rCommentChar cs1
  . rNewline n2

pComment :: Parser Comment
pComment
  = Comment
  <$> pHash
  <*> many pCommentChar
  <*> pNewline
  <?> "comment"

data DigitsFour = DigitsFour D9z D9z D9z D9z
  deriving (Eq, Ord, Show)

pDigitsFour :: Parser DigitsFour
pDigitsFour = DigitsFour <$> pD9z <*> pD9z <*> pD9z <*> pD9z

rDigitsFour :: DigitsFour -> ShowS
rDigitsFour (DigitsFour d0 d1 d2 d3) = rD9z d0
  . rD9z d1 . rD9z d2 . rD9z d3

data Digits1or2 = Digits1or2 D9z (Maybe D9z)
  deriving (Eq, Ord, Show)

pDigits1or2 :: Parser Digits1or2
pDigits1or2 = Digits1or2 <$> pD9z <*> optional pD9z

rDigits1or2 :: Digits1or2 -> ShowS
rDigits1or2 (Digits1or2 d0 m1) = rD9z d0 . rMaybe rD9z m1

data Colon = Colon
  deriving (Eq, Ord, Show)

pColon :: Parser Colon
pColon = Colon <$ char ':'

rColon :: Colon -> ShowS
rColon Colon = (':':)

pD1z :: Parser D1z
pD1z = D1z'0 <$ char '0' <|> D1z'1 <$ char '1'

rD1z :: D1z -> ShowS
rD1z x = case x of
  D1z'0 -> ('0':)
  D1z'1 -> ('1':)

pD2z :: Parser D2z
pD2z = D2z'0 <$ char '0' <|> D2z'1 <$ char '1'
  <|> D2z'2 <$ char '2'

rD2z :: D2z -> ShowS
rD2z x = case x of
  D2z'0 -> ('0':)
  D2z'1 -> ('1':)
  D2z'2 -> ('2':)

pD3z :: Parser D3z
pD3z = D3z'0 <$ char '0'
  <|> D3z'1 <$ char '1'
  <|> D3z'2 <$ char '2'
  <|> D3z'3 <$ char '3'

rD3z :: D3z -> ShowS
rD3z x = case x of
  D3z'0 -> ('0':)
  D3z'1 -> ('1':)
  D3z'2 -> ('2':)
  D3z'3 -> ('3':)

pD5z :: Parser D5z
pD5z = D5z'0 <$ char '0'
  <|> D5z'1 <$ char '1'
  <|> D5z'2 <$ char '2'
  <|> D5z'3 <$ char '3'
  <|> D5z'4 <$ char '4'
  <|> D5z'5 <$ char '5'

rD5z :: D5z -> ShowS
rD5z x = case x of
  D5z'0 -> ('0':)
  D5z'1 -> ('1':)
  D5z'2 -> ('2':)
  D5z'3 -> ('3':)
  D5z'4 -> ('4':)
  D5z'5 -> ('5':)

data HoursA
  = H0to19a (Maybe D1z) D9z
  | H20to23a Two D3z
  deriving (Eq, Ord, Show)

pHoursA :: Parser HoursA
pHoursA
  = H0to19a <$> optional pD1z <*> pD9z
  <|> H20to23a <$> pTwo <*> pD3z

rHoursA :: HoursA -> ShowS
rHoursA x = case x of
  H0to19a m0 d1 -> rMaybe rD1z m0 . rD9z d1
  H20to23a t0 d1 -> rTwo t0 . rD3z d1

pZeroTo59 :: Parser ZeroTo59
pZeroTo59 = ZeroTo59 <$> optional pD5z <*> pD9z

rZeroTo59 :: ZeroTo59 -> ShowS
rZeroTo59 (ZeroTo59 m0 d1) = rMaybe rD5z m0 . rD9z d1

pMinutes :: Parser Minutes
pMinutes = Minutes <$> pZeroTo59

rMinutes :: Minutes -> ShowS
rMinutes (Minutes x) = rZeroTo59 x

pSeconds :: Parser Seconds
pSeconds = Seconds <$> pZeroTo59

rSeconds :: Seconds -> ShowS
rSeconds (Seconds x) = rZeroTo59 x

data TimeA = TimeA
  HoursA Colon Minutes (Maybe (Colon, Seconds))
  deriving (Eq, Ord, Show)

pTimeA :: Parser TimeA
pTimeA = TimeA <$> pHoursA <*> pColon <*> pMinutes
    <*> optional (try ((,) <$> pColon <*> pSeconds))

rTimeA :: TimeA -> ShowS
rTimeA (TimeA d0 c1 d2 m3) = rHoursA d0 . rColon c1
  . rMinutes d2 . rMaybe (\(c3, d4) -> rColon c3 . rSeconds d4) m3

pZone :: Parser Zone
pZone = Zone <$> pPluMin <*> pD2z <*> pD3z <*> pD9z <*> pD9z

rZone :: Zone -> ShowS
rZone (Zone p0 d1 d2 d3 d4)
  = rPluMin p0
  . rD2z d1
  . rD3z d2
  . rD9z d3
  . rD9z d4

data ZoneA = ZoneA Backtick Zone
  deriving (Eq, Ord, Show)

rZoneA :: ZoneA -> ShowS
rZoneA (ZoneA b0 z1)
  = rBacktick b0 . rZone z1

pZoneA :: Parser ZoneA
pZoneA = ZoneA <$> pBacktick <*> pZone

data DoubleQuote = DoubleQuote
  deriving (Eq, Ord, Show)

pDoubleQuote :: Parser DoubleQuote
pDoubleQuote = DoubleQuote <$ char '"'

rDoubleQuote :: DoubleQuote -> ShowS
rDoubleQuote DoubleQuote = ('"':)

data Backslash = Backslash
  deriving (Eq, Ord, Show)

pBackslash :: Parser Backslash
pBackslash = Backslash <$ char '\\'

rBackslash :: Backslash -> ShowS
rBackslash Backslash = ('\\':)

data White
  = Space
  | Tab
  | WhiteNewline
  | WhiteComment Comment
  deriving (Eq, Ord, Show)

pWhite :: Parser White
pWhite = Space <$ char ' ' <|> Tab <$ char '\t'
  <|> WhiteNewline <$ char '\n'
  <|> WhiteComment <$> pComment
  <?> "whitespace or comment"

rWhite :: White -> ShowS
rWhite w = case w of
  Space -> (' ':)
  Tab -> ('\t':)
  WhiteNewline -> ('\n':)
  WhiteComment c -> rComment c

data Whites = Whites White [White]
  deriving (Eq, Ord, Show)

rWhites :: Whites -> ShowS
rWhites (Whites w ws) = rWhite w . rList rWhite ws

pWhites :: Parser Whites
pWhites = Whites <$> pWhite <*> many pWhite

-- | Something that might be followed by spaces.
data Fs a = Fs a [White]
  deriving (Eq, Ord, Show)

rFs :: (a -> ShowS) -> Fs a -> ShowS
rFs f (Fs a mw) = f a . rList rWhite mw

fsHasWhite :: Fs a -> Bool
fsHasWhite (Fs _ m) = not (null m)

instance Functor Fs where
  fmap f (Fs a w) = Fs (f a) w

pFs :: Parser a -> Parser (Fs a)
pFs p = Fs <$> p <*> many pWhite

data EscPayload
  = EscBackslash
  | EscNewline
  | EscQuote
  | EscGap Whites Backslash
  deriving (Eq, Ord, Show)

pEscPayload :: Parser EscPayload
pEscPayload = EscBackslash <$ char '\\'
  <|> EscNewline <$ char 'n'
  <|> EscQuote <$ char '"'
  <|> EscGap <$> pWhites <*> pBackslash

rEscPayload :: EscPayload -> ShowS
rEscPayload x = case x of
  EscBackslash -> ('\\':)
  EscNewline -> ('n':)
  EscQuote -> ('"':)
  EscGap w b -> rWhites w . rBackslash b

data EscSeq = EscSeq Backslash EscPayload
  deriving (Eq, Ord, Show)

pEscSeq :: Parser EscSeq
pEscSeq = EscSeq <$> pBackslash <*> pEscPayload

rEscSeq :: EscSeq -> ShowS
rEscSeq (EscSeq b0 p1) = rBackslash b0 . rEscPayload p1

newtype QuotedChar = QuotedChar (Either NonEscapedChar EscSeq)
  deriving (Eq, Ord, Show)

rQuotedChar :: QuotedChar -> ShowS
rQuotedChar (QuotedChar ei) = case ei of
  Left nes -> rNonEscapedChar nes
  Right sq -> rEscSeq sq

pQuotedChar :: Parser QuotedChar
pQuotedChar = QuotedChar <$>
  ( Left <$> pNonEscapedChar <|> Right <$> pEscSeq)

data QuotedString = QuotedString DoubleQuote [QuotedChar] DoubleQuote
  deriving (Eq, Ord, Show)

pQuotedString :: Parser QuotedString
pQuotedString = QuotedString
  <$> pDoubleQuote
  <*> many (try pQuotedChar)
  <*> pDoubleQuote

rQuotedString :: QuotedString -> ShowS
rQuotedString (QuotedString q0 cs1 q2)
  = rDoubleQuote q0 . rList rQuotedChar cs1 . rDoubleQuote q2

data UnquotedString
  = UnquotedString [D9z] USCharNonDigit [Either USCharNonDigit D9z]
  deriving (Eq, Ord, Show)

pUnquotedString :: Parser UnquotedString
pUnquotedString = UnquotedString
  <$> many pD9z <*> pUSCharNonDigit
  <*> many (Left <$> pUSCharNonDigit <|> Right <$> pD9z)


rUnquotedString :: UnquotedString -> ShowS
rUnquotedString (UnquotedString ds0 c1 ei2) =
  rList rD9z ds0 . rUSCharNonDigit c1
  . rList (either rUSCharNonDigit rD9z) ei2

-- Parsing the Trio
--
-- Whitespace is a space, tab, newline, or comment.
--
-- The Debit or Credit, if there is one, always comes first.  It is
-- followed by an optional run of whitespace.
--
-- If there is a Debit or Credit, the quantity must be Brim.
-- Otherwise, it must be Nil.
--
-- The commodity can be a quoted string, which is most flexible.  It
-- can also be a non-quoted string.  In this case, if the unquoted
-- commodity is on the left, it may contain only non-digit characters.
-- If the unquoted commodity is on the right, it must start with a
-- non-digit character, but following characters may be digits.

data UnquotedCommodityOnLeft
  = UnquotedCommodityOnLeft USCharNonDigit [USCharNonDigit]
  deriving (Eq, Ord, Show)

pUnquotedCommodityOnLeft :: Parser UnquotedCommodityOnLeft
pUnquotedCommodityOnLeft = UnquotedCommodityOnLeft
  <$> pUSCharNonDigit <*> many pUSCharNonDigit

rUnquotedCommodityOnLeft :: UnquotedCommodityOnLeft -> ShowS
rUnquotedCommodityOnLeft (UnquotedCommodityOnLeft c0 ls1)
  = rUSCharNonDigit c0 . rList rUSCharNonDigit ls1

data UnquotedCommodityOnRight
  = UnquotedCommodityOnRight USCharNonDigit [USCharNonDigit]
  deriving (Eq, Ord, Show)

pUnquotedCommodityOnRight :: Parser UnquotedCommodityOnRight
pUnquotedCommodityOnRight = UnquotedCommodityOnRight
  <$> pUSCharNonDigit
  <*> many pUSCharNonDigit

rUnquotedCommodityOnRight :: UnquotedCommodityOnRight -> ShowS
rUnquotedCommodityOnRight (UnquotedCommodityOnRight d0 ls1)
  = rUSCharNonDigit d0 . rList rUSCharNonDigit ls1

newtype CommodityOnLeftA
  = CommodityOnLeftA (Either UnquotedCommodityOnLeft QuotedCommodity)
  deriving (Eq, Ord, Show)

pCommodityOnLeftA :: Parser CommodityOnLeftA
pCommodityOnLeftA = CommodityOnLeftA
  <$> (Left <$> pUnquotedCommodityOnLeft <|> Right <$> pQuotedCommodity)

rCommodityOnLeftA :: CommodityOnLeftA -> ShowS
rCommodityOnLeftA (CommodityOnLeftA ei)
  = either rUnquotedCommodityOnLeft rQuotedCommodity ei

newtype CommodityOnRightA
  = CommodityOnRightA (Either UnquotedCommodityOnRight QuotedCommodity)
  deriving (Eq, Ord, Show)

pCommodityOnRightA :: Parser CommodityOnRightA
pCommodityOnRightA = CommodityOnRightA
  <$> (Left <$> pUnquotedCommodityOnRight <|> Right <$> pQuotedCommodity)

rCommodityOnRightA :: CommodityOnRightA -> ShowS
rCommodityOnRightA (CommodityOnRightA ei)
  = either rUnquotedCommodityOnRight rQuotedCommodity ei

newtype QuotedCommodity = QuotedCommodity QuotedString
  deriving (Eq, Ord, Show)

pQuotedCommodity :: Parser QuotedCommodity
pQuotedCommodity = QuotedCommodity <$> pQuotedString

rQuotedCommodity :: QuotedCommodity -> ShowS
rQuotedCommodity (QuotedCommodity q0) = rQuotedString q0

newtype CommodityA
  = CommodityA (Either UnquotedCommodityOnRight QuotedCommodity)
  deriving (Eq, Ord, Show)

pCommodityA :: Parser CommodityA
pCommodityA = CommodityA <$>
  (Left <$> pUnquotedCommodityOnRight <|> Right <$> pQuotedCommodity)

rCommodityA :: CommodityA -> ShowS
rCommodityA (CommodityA ei)
  = either rUnquotedCommodityOnRight rQuotedCommodity ei

data Backtick = Backtick
  deriving (Eq, Ord, Show)

pBacktick :: Parser Backtick
pBacktick = Backtick <$ char '`'

rBacktick :: Backtick -> ShowS
rBacktick Backtick = ('`':)

data NonNeutral
  = NonNeutralRadCom Backtick (Brim RadCom)
  | NonNeutralRadPer (Brim RadPer)
  deriving (Eq, Ord, Show)

pNonNeutral :: Parser NonNeutral
pNonNeutral
  = NonNeutralRadCom <$> pBacktick <*> pBrim pRadixRadCom pRadCom
  <|> NonNeutralRadPer <$> pBrim pRadixRadPer pRadPer

rNonNeutral :: NonNeutral -> ShowS
rNonNeutral x = case x of
  NonNeutralRadCom b0 b1 -> rBacktick b0 . rBrim rRadixRadCom rRadCom b1
  NonNeutralRadPer b0 -> rBrim rRadixRadPer rRadPer b0

data NeutralOrNon
  = NeutralOrNonRadCom Backtick (Either (Nil RadCom) (Brim RadCom))
  | NeutralOrNonRadPer (Either (Nil RadPer) (Brim RadPer))
  deriving (Eq, Ord, Show)

pNeutralOrNon :: Parser NeutralOrNon
pNeutralOrNon
  = NeutralOrNonRadCom <$> pBacktick <*>
      (Left <$> try (pNil pRadixRadCom pRadCom)
        <|> Right <$> try (pBrim pRadixRadCom pRadCom))
  <|> NeutralOrNonRadPer <$>
      (Left <$> try (pNil pRadixRadPer pRadPer)
        <|> Right <$> try (pBrim pRadixRadPer pRadPer))

rNeutralOrNon :: NeutralOrNon -> ShowS
rNeutralOrNon x = case x of
  NeutralOrNonRadCom b0 ei -> rBacktick b0
    . either (rNil rRadixRadCom rRadCom) (rBrim rRadixRadCom rRadCom) ei
  NeutralOrNonRadPer ei -> either (rNil rRadixRadPer rRadPer)
                                  (rBrim rRadixRadPer rRadPer) ei

data Neutral
  = NeuCom Backtick (Nil RadCom)
  | NeuPer (Nil RadPer)
  deriving (Eq, Ord, Show)

pNeutral :: Parser Neutral
pNeutral
  = NeuCom <$> pBacktick <*> pNil pRadixRadCom pRadCom
  <|> NeuPer <$> pNil pRadixRadPer pRadPer

rNeutral :: Neutral -> ShowS
rNeutral neu = case neu of
  NeuCom b0 n1 -> rBacktick b0 . rNil rRadixRadCom rRadCom n1
  NeuPer n0 -> rNil rRadixRadPer rRadPer n0

data IntegerA = IntegerA (Either Zero (Maybe PluMin, D9, [D9z]))
  deriving (Eq, Ord, Show)

pIntegerA :: Parser IntegerA
pIntegerA = IntegerA <$>
  (Left <$> pZero <|> Right <$> ((,,) <$> optional pPluMin
                                      <*> pD9 <*> many pD9z))

rIntegerA :: IntegerA -> ShowS
rIntegerA (IntegerA ei) = case ei of
  Left z -> rZero z
  Right (m0, n1, ds2) -> rMaybe rPluMin m0 . rD9 n1 . rList rD9z ds2

-- # Lexeme productions.  These include any trailing whitespace.

-- | Trio.  There is nothing corresponding to 'Penny.Trio.E'
-- as this would screw up the spacing, and generally productions in
-- the AST should actually produce something.  Instead,
-- 'Penny.Trio.E' is indicated by the absense of any 'TrioA'.
data TrioA
  = QcCyOnLeftA (Fs Pole) (Fs CommodityOnLeftA) (Fs NonNeutral)
  -- ^ Non neutral, commodity on left
  | QcCyOnRightA (Fs Pole) (Fs NonNeutral) (Fs CommodityOnRightA)
  -- ^ Non neutral, commodity on right
  | QSided (Fs Pole) (Fs NonNeutral)
  -- ^ Qty with side only
  | QUnsided (Fs Neutral)
  -- ^ Qty, with no side
  | SCA (Fs Pole) (Fs CommodityA)
  -- ^ Side and commodity
  | SA (Fs Pole)
  -- ^ Side only
  | UcCyOnLeftA (Fs CommodityOnLeftA) (Fs NonNeutral)
  -- ^ Unsigned quantity and commodity only, commodity on left
  | UcCyOnRightA (Fs NonNeutral) (Fs CommodityOnRightA)
  -- ^ Unsigned quantity and commodity only, commodity on right
  | UA (Fs NonNeutral)
  -- ^ Non-sided non-neutral quantity only
  | CA (Fs CommodityA)
  -- ^ Commodity only
  deriving (Eq, Ord, Show)

pTrioA :: Parser TrioA
pTrioA
  =   try (QcCyOnLeftA <$> pFs pSide <*> pFs pCommodityOnLeftA <*> pFs pNonNeutral)
  <|> try (QcCyOnRightA <$> pFs pSide <*> pFs pNonNeutral <*> pFs pCommodityOnRightA)
  <|> try (QSided <$> pFs pSide <*> pFs pNonNeutral)
  <|> try (QUnsided <$> pFs pNeutral)
  <|> try (SCA <$> pFs pSide <*> pFs pCommodityA)
  <|> try (SA <$> pFs pSide)
  <|> try (UcCyOnLeftA <$> pFs pCommodityOnLeftA <*> pFs pNonNeutral)
  <|> try (UcCyOnRightA <$> pFs pNonNeutral <*> pFs pCommodityOnRightA)
  <|> try (UA <$> pFs pNonNeutral)
  <|> try (CA <$> pFs pCommodityA)

rTrioA :: TrioA -> ShowS
rTrioA x = case x of
  QcCyOnLeftA s0 c1 n1 -> rFs rSide s0 . rFs rCommodityOnLeftA c1
    . rFs rNonNeutral n1
  QcCyOnRightA s0 n1 c2 -> rFs rSide s0 . rFs rNonNeutral n1
    . rFs rCommodityOnRightA c2
  QSided s0 n1 -> rFs rSide s0 . rFs rNonNeutral n1
  QUnsided n0 -> rFs rNeutral n0
  SCA s0 c1 -> rFs rSide s0 . rFs rCommodityA c1
  SA s -> rFs rSide s
  UcCyOnLeftA c0 n1 -> rFs rCommodityOnLeftA c0 . rFs rNonNeutral n1
  UcCyOnRightA n0 c1 -> rFs rNonNeutral n0 . rFs rCommodityOnRightA c1
  UA n -> rFs rNonNeutral n
  CA c -> rFs rCommodityA c

data OpenSquare = OpenSquare
  deriving (Eq, Ord, Show)

pOpenSquare :: Parser OpenSquare
pOpenSquare = OpenSquare <$ char '['

rOpenSquare :: OpenSquare -> ShowS
rOpenSquare OpenSquare = ('[':)

data CloseSquare = CloseSquare
  deriving (Eq, Ord, Show)

pCloseSquare :: Parser CloseSquare
pCloseSquare = CloseSquare <$ char ']'

rCloseSquare :: CloseSquare -> ShowS
rCloseSquare CloseSquare = (']':)

data ScalarA
  = ScalarUnquotedString (Fs UnquotedString)
  | ScalarQuotedString (Fs QuotedString)
  | ScalarDate (Fs DateA)
  | ScalarTime (Fs TimeA)
  | ScalarZone (Fs ZoneA)
  | ScalarInt (Fs IntegerA)
  deriving (Eq, Ord, Show)

pScalarA :: Parser ScalarA
pScalarA
  =   try (ScalarDate <$> pFs pDateA)
  <|> try (ScalarTime <$> pFs pTimeA)
  <|> try (ScalarZone <$> pFs pZoneA)
  <|> try (ScalarInt <$> pFs pIntegerA)
  <|> try (ScalarQuotedString <$> pFs pQuotedString)
  <|> try (ScalarUnquotedString <$> pFs pUnquotedString)

rScalarA :: ScalarA -> ShowS
rScalarA sclrA = case sclrA of
  ScalarUnquotedString x -> rFs rUnquotedString x
  ScalarQuotedString x -> rFs rQuotedString x
  ScalarDate x -> rFs rDateA x
  ScalarTime x -> rFs rTimeA x
  ScalarZone x -> rFs rZoneA x
  ScalarInt x -> rFs rIntegerA x

data BracketedForest = BracketedForest
  (Fs OpenSquare) (Maybe ForestA) (Fs CloseSquare)
  deriving (Eq, Ord, Show)

data ForestA = ForestA (Fs TreeA) [(Fs CommaA, Fs TreeA)]
  deriving (Eq, Ord, Show)

pForestA :: Parser ForestA
pForestA = ForestA <$> pFs pTreeA
  <*> many ((,) <$> pFs pCommaA <*> pFs pTreeA)

rForestA :: ForestA -> ShowS
rForestA (ForestA t0 ls1)
  = rFs rTreeA t0 . rList (\(c, t) -> rFs rCommaA c . rFs rTreeA t) ls1

pBracketedForest :: Parser BracketedForest
pBracketedForest = BracketedForest <$> pFs pOpenSquare
  <*> optional pForestA <*> pFs pCloseSquare

rBracketedForest :: BracketedForest -> ShowS
rBracketedForest (BracketedForest b0 m1 b2) = rFs rOpenSquare b0
  . rMaybe rForestA m1 . rFs rCloseSquare b2

data TreeA
  = TreeScalarFirst (Located ScalarA) (Maybe BracketedForest)
  | TreeForestFirst BracketedForest (Maybe (Located ScalarA))
  deriving (Eq, Ord, Show)

pTreeA :: Parser TreeA
pTreeA
  = TreeScalarFirst <$> pLocated pScalarA <*> optional pBracketedForest
  <|> TreeForestFirst <$> pBracketedForest
      <*> optional (pLocated pScalarA)

rTreeA :: TreeA -> ShowS
rTreeA (TreeScalarFirst s1 m2) = rLocated rScalarA s1
  . rMaybe rBracketedForest m2
rTreeA (TreeForestFirst f1 m2) = rBracketedForest f1
  . rMaybe (rLocated rScalarA) m2

newtype TopLineA = TopLineA ForestA
  deriving (Eq, Ord, Show)

pTopLineA :: Parser TopLineA
pTopLineA = TopLineA <$> pForestA

rTopLineA :: TopLineA -> ShowS
rTopLineA (TopLineA frst) = rForestA frst

data PostingA
  = PostingTrioFirst (Located TrioA) (Maybe BracketedForest)
  | PostingNoTrio BracketedForest
  deriving (Eq, Ord, Show)

pPostingA :: Parser PostingA
pPostingA
  = PostingTrioFirst <$> pLocated pTrioA
                     <*> optional pBracketedForest
  <|> PostingNoTrio <$> pBracketedForest

rPostingA :: PostingA -> ShowS
rPostingA pstg = case pstg of
  PostingTrioFirst t0 m1 -> rLocated rTrioA t0
    . rMaybe rBracketedForest m1
  PostingNoTrio b0 -> rBracketedForest b0

data OpenCurly = OpenCurly
  deriving (Eq, Ord, Show)

pOpenCurly :: Parser OpenCurly
pOpenCurly = OpenCurly <$ char '{'

rOpenCurly :: OpenCurly -> ShowS
rOpenCurly OpenCurly = ('{':)

data CloseCurly = CloseCurly
  deriving (Eq, Ord, Show)

pCloseCurly :: Parser CloseCurly
pCloseCurly = CloseCurly <$ char '}'

rCloseCurly :: CloseCurly -> ShowS
rCloseCurly CloseCurly = ('}':)

data CommaA = CommaA
  deriving (Eq, Ord, Show)

pCommaA :: Parser CommaA
pCommaA = CommaA <$ char ','

rCommaA :: CommaA -> ShowS
rCommaA CommaA = (',':)

data PostingList
  = PostingList (Located PostingA) [(Fs Semicolon, Located PostingA)]
  deriving (Eq, Ord, Show)

pPostingList :: Parser PostingList
pPostingList = PostingList <$> pLocated pPostingA
  <*> many ((,) <$> pFs pSemicolon <*> pLocated pPostingA)

rPostingList :: PostingList -> ShowS
rPostingList (PostingList p1 ps)
  = rLocated rPostingA p1
  . rList (\(s, p) -> rFs rSemicolon s . rLocated rPostingA p) ps

data PostingsA = PostingsA (Fs OpenCurly)
  (Maybe PostingList) (Fs CloseCurly)
  deriving (Eq, Ord, Show)

pPostingsA :: Parser PostingsA
pPostingsA = PostingsA <$> pFs pOpenCurly
  <*> optional pPostingList <*> pFs pCloseCurly

rPostingsA :: PostingsA -> ShowS
rPostingsA (PostingsA c0 m1 c2)
  = rFs rOpenCurly c0 . rMaybe rPostingList m1
  . rFs rCloseCurly c2

data Semicolon = Semicolon
  deriving (Eq, Ord, Show)

pSemicolon :: Parser Semicolon
pSemicolon = Semicolon <$ char ';'

rSemicolon :: Semicolon -> ShowS
rSemicolon Semicolon = (';':)

data TransactionA = TransactionA (Maybe TopLineA) PostingsA
  deriving (Eq, Ord, Show)

pTransactionA :: Parser TransactionA
pTransactionA = TransactionA <$> optional pTopLineA <*> pPostingsA

rTransactionA :: TransactionA -> ShowS
rTransactionA (TransactionA m0 p1)
  = rMaybe rTopLineA m0 . rPostingsA p1

data AtSign = AtSign
  deriving (Eq, Ord, Show)

pAtSign :: Parser AtSign
pAtSign = AtSign <$ char '@'

rAtSign :: AtSign -> ShowS
rAtSign AtSign = ('@':)

data ExchA
  = ExchANeutral (Fs Neutral)
  | ExchANonNeutral (Maybe (Fs PluMin)) (Fs NonNeutral)
  deriving (Eq, Ord, Show)

pExchA :: Parser ExchA
pExchA = ExchANeutral <$> pFs pNeutral
  <|> ExchANonNeutral <$> optional (pFs pPluMin) <*> pFs pNonNeutral

rExchA :: ExchA -> ShowS
rExchA exch = case exch of
  ExchANeutral n -> rFs rNeutral n
  ExchANonNeutral mayFs nn -> rMaybe (rFs rPluMin) mayFs . rFs rNonNeutral nn

data CyExch
  = CyExchCy (Fs CommodityA) ExchA
  | CyExchA ExchA (Fs CommodityA)
  deriving (Eq, Ord, Show)

pCyExch :: Parser CyExch
pCyExch = CyExchCy <$> (pFs pCommodityA) <*> pExchA
  <|> CyExchA <$> pExchA <*> pFs pCommodityA

rCyExch :: CyExch -> ShowS
rCyExch exch = case exch of
  CyExchCy cy0 q1 -> rFs rCommodityA cy0 . rExchA q1
  CyExchA q0 c1 -> rExchA q0 . rFs rCommodityA c1

data PriceA = PriceA (Fs AtSign) DateA Whites
  (Maybe (TimeA, Whites)) (Maybe (ZoneA, Whites))
  CommodityA Whites CyExch
  deriving (Eq, Ord, Show)

pPriceA :: Parser PriceA
pPriceA = PriceA <$> pFs pAtSign <*> pDateA <*> pWhites
  <*> optional (try ((,) <$> pTimeA <*> pWhites))
  <*> optional (try ((,) <$> pZoneA <*> pWhites))
  <*> pCommodityA <*> pWhites <*> pCyExch

rPriceA :: PriceA -> ShowS
rPriceA (PriceA a0 l1 w2 m3 m4 c5 w6 e7)
  = rFs rAtSign a0
  . rDateA l1
  . rWhites w2
  . rMaybe (\(t, w) -> rTimeA t . rWhites w) m3
  . rMaybe (\(z, w) -> rZoneA z . rWhites w) m4
  . rCommodityA c5
  . rWhites w6
  . rCyExch e7

data FileItem = FileItem (Located (Either PriceA TransactionA))
  deriving (Eq, Ord, Show)

pFileItem :: Parser FileItem
pFileItem = FileItem
  <$> pLocated ((Left <$> pPriceA) <|> (Right <$> pTransactionA))

rFileItem :: FileItem -> ShowS
rFileItem (FileItem l0) =
  rLocated (either rPriceA rTransactionA) l0

-- | Unlike every other production in this module, 'Ast' may produce
-- an empty input.
data Ast = Ast [White] [FileItem]
  deriving (Eq, Ord, Show)

pAst :: Parser Ast
pAst = Ast <$> many pWhite <*> many pFileItem

rAst :: Ast -> ShowS
rAst (Ast w f) = rList rWhite w . rList rFileItem f
-}
