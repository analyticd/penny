module Penny.Copper.Ast where

import Control.Applicative
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)
import Text.ParserCombinators.UU.Core
import Text.ParserCombinators.UU.Derived
import Penny.Lincoln
import Penny.Copper.Terminals
import Penny.Copper.Parser
import Penny.Copper.LincolnTypes
import Penny.Copper.Date
import Data.List (intersperse)

data Located a = Located LineColPosA a
  deriving (Eq, Ord, Show)

instance Functor Located where
  fmap f (Located l1 a) = Located l1 (f a)

pLocated :: ParserL a -> ParserL (Located a)
pLocated p = Located <$> pPos <*> p

rLocated :: (a -> ShowS) -> Located a -> ShowS
rLocated f (Located _ a) = f a

-- | Something that might be followed by spaces.
data Fs a = Fs a (Maybe Whites)
  deriving (Eq, Ord, Show)

rFs :: (a -> ShowS) -> Fs a -> ShowS
rFs f (Fs a mw) = f a . rMaybe rWhites mw

fsHasWhite :: Fs a -> Bool
fsHasWhite (Fs _ m) = maybe False (const True) m

instance Functor Fs where
  fmap f (Fs a w) = Fs (f a) w

pFs :: ParserL a -> ParserL (Fs a)
pFs p = Fs <$> p <*> optional pWhites

-- | Something that might be preceded by spaces.
data Bs a = Bs (Maybe Whites) a
  deriving (Eq, Ord, Show)

bsHasWhite :: Bs a -> Bool
bsHasWhite (Bs m _) = maybe False (const True) m

instance Functor Bs where
  fmap f (Bs w a) = Bs w (f a)

pBs :: ParserL a -> ParserL (Bs a)
pBs p = Bs <$> optional pWhites <*> p

rBs :: (a -> ShowS) -> Bs a -> ShowS
rBs f (Bs w a) = rMaybe rWhites w . f a

-- | Octothorpe
data Hash = Hash
  deriving (Eq, Ord, Show)

pHash :: ParserL Hash
pHash = Hash <$ pSym '#'

rHash :: Hash -> ShowS
rHash Hash = ('#':)

data Newline = Newline
  deriving (Eq, Ord, Show)

pNewline :: ParserL Newline
pNewline = Newline <$ pSym '\n'

rNewline :: Newline -> ShowS
rNewline Newline = ('\n':)

data Comment = Comment Hash [CommentChar] Newline
  deriving (Eq, Ord, Show)

rComment :: Comment -> ShowS
rComment (Comment h0 cs1 n2) = rHash h0 . rList rCommentChar cs1
  . rNewline n2

pComment :: ParserL Comment
pComment
  = Comment
  <$> pHash
  <*> many pCommentChar
  <*> pNewline
  <?> "comment"

data DigitsFour = DigitsFour D9z D9z D9z D9z
  deriving (Eq, Ord, Show)

pDigitsFour :: ParserL DigitsFour
pDigitsFour = DigitsFour <$> pD9z <*> pD9z <*> pD9z <*> pD9z

rDigitsFour :: DigitsFour -> ShowS
rDigitsFour (DigitsFour d0 d1 d2 d3) = rD9z d0
  . rD9z d1 . rD9z d2 . rD9z d3

data Digits1or2 = Digits1or2 D9z (Maybe D9z)
  deriving (Eq, Ord, Show)

pDigits1or2 :: ParserL Digits1or2
pDigits1or2 = Digits1or2 <$> pD9z <*> optional pD9z

rDigits1or2 :: Digits1or2 -> ShowS
rDigits1or2 (Digits1or2 d0 m1) = rD9z d0 . rMaybe rD9z m1

data Colon = Colon
  deriving (Eq, Ord, Show)

pColon :: ParserL Colon
pColon = Colon <$ pSym ':'

rColon :: Colon -> ShowS
rColon Colon = (':':)

pD1z :: ParserL D1z
pD1z = D1z'0 <$ pSym '0' <|> D1z'1 <$ pSym '1'

rD1z :: D1z -> ShowS
rD1z x = case x of
  D1z'0 -> ('0':)
  D1z'1 -> ('1':)

pD2z :: ParserL D2z
pD2z = D2z'0 <$ pSym '0' <|> D2z'1 <$ pSym '1'
  <|> D2z'2 <$ pSym '2'

rD2z :: D2z -> ShowS
rD2z x = case x of
  D2z'0 -> ('0':)
  D2z'1 -> ('1':)
  D2z'2 -> ('2':)

pD3z :: ParserL D3z
pD3z = D3z'0 <$ pSym '0'
  <|> D3z'1 <$ pSym '1'
  <|> D3z'2 <$ pSym '2'
  <|> D3z'3 <$ pSym '3'

rD3z :: D3z -> ShowS
rD3z x = case x of
  D3z'0 -> ('0':)
  D3z'1 -> ('1':)
  D3z'2 -> ('2':)
  D3z'3 -> ('3':)

pD5z :: ParserL D5z
pD5z = D5z'0 <$ pSym '0'
  <|> D5z'1 <$ pSym '1'
  <|> D5z'2 <$ pSym '2'
  <|> D5z'3 <$ pSym '3'
  <|> D5z'4 <$ pSym '4'
  <|> D5z'5 <$ pSym '5'

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

pHoursA :: ParserL HoursA
pHoursA
  = H0to19a <$> optional pD1z <*> pD9z
  <|> H20to23a <$> pTwo <*> pD3z

rHoursA :: HoursA -> ShowS
rHoursA x = case x of
  H0to19a m0 d1 -> rMaybe rD1z m0 . rD9z d1
  H20to23a t0 d1 -> rTwo t0 . rD3z d1

pZeroTo59 :: ParserL ZeroTo59
pZeroTo59 = ZeroTo59 <$> optional pD5z <*> pD9z

rZeroTo59 :: ZeroTo59 -> ShowS
rZeroTo59 (ZeroTo59 m0 d1) = rMaybe rD5z m0 . rD9z d1

pMinutes :: ParserL Minutes
pMinutes = Minutes <$> pZeroTo59

rMinutes :: Minutes -> ShowS
rMinutes (Minutes x) = rZeroTo59 x

pSeconds :: ParserL Seconds
pSeconds = Seconds <$> pZeroTo59

rSeconds :: Seconds -> ShowS
rSeconds (Seconds x) = rZeroTo59 x

data TimeA = TimeA
  HoursA Colon Minutes (Maybe (Colon, Seconds))
  deriving (Eq, Ord, Show)

pTimeA :: ParserL TimeA
pTimeA = TimeA <$> pHoursA <*> pColon <*> pMinutes
    <*> optional ((,) <$> pColon <*> pSeconds)

rTimeA :: TimeA -> ShowS
rTimeA (TimeA d0 c1 d2 m3) = rHoursA d0 . rColon c1
  . rMinutes d2 . rMaybe (\(c3, d4) -> rColon c3 . rSeconds d4) m3

pZone :: ParserL Zone
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

pZoneA :: ParserL ZoneA
pZoneA = ZoneA <$> pBacktick <*> pZone

data DoubleQuote = DoubleQuote
  deriving (Eq, Ord, Show)

pDoubleQuote :: ParserL DoubleQuote
pDoubleQuote = DoubleQuote <$ pSym '"'

rDoubleQuote :: DoubleQuote -> ShowS
rDoubleQuote DoubleQuote = ('"':)

data Backslash = Backslash
  deriving (Eq, Ord, Show)

pBackslash :: ParserL Backslash
pBackslash = Backslash <$ pSym '\\'

rBackslash :: Backslash -> ShowS
rBackslash Backslash = ('\\':)

data White
  = Space
  | Tab
  | WhiteNewline
  | WhiteComment Comment
  deriving (Eq, Ord, Show)

pWhite :: ParserL White
pWhite = Space <$ pSym ' ' <|> Tab <$ pSym '\t'
  <|> WhiteNewline <$ pSym '\n'
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

pWhites :: ParserL Whites
pWhites = Whites <$> pWhite <*> many pWhite

data EscPayload
  = EscBackslash
  | EscNewline
  | EscQuote
  | EscGap Whites Backslash
  deriving (Eq, Ord, Show)

pEscPayload :: ParserL EscPayload
pEscPayload = EscBackslash <$ pSym '\\'
  <|> EscNewline <$ pSym 'n'
  <|> EscQuote <$ pSym '"'
  <|> EscGap <$> pWhites <*> pBackslash

rEscPayload :: EscPayload -> ShowS
rEscPayload x = case x of
  EscBackslash -> ('\\':)
  EscNewline -> ('n':)
  EscQuote -> ('"':)
  EscGap w b -> rWhites w . rBackslash b

data EscSeq = EscSeq Backslash EscPayload
  deriving (Eq, Ord, Show)

pEscSeq :: ParserL EscSeq
pEscSeq = EscSeq <$> pBackslash <*> pEscPayload

rEscSeq :: EscSeq -> ShowS
rEscSeq (EscSeq b0 p1) = rBackslash b0 . rEscPayload p1

newtype QuotedChar = QuotedChar (Either NonEscapedChar EscSeq)
  deriving (Eq, Ord, Show)

rQuotedChar :: QuotedChar -> ShowS
rQuotedChar (QuotedChar ei) = case ei of
  Left nes -> rNonEscapedChar nes
  Right sq -> rEscSeq sq

pQuotedChar :: ParserL QuotedChar
pQuotedChar = QuotedChar <$>
  ( Left <$> pNonEscapedChar <|> Right <$> pEscSeq)

data QuotedString = QuotedString DoubleQuote [QuotedChar] DoubleQuote
  deriving (Eq, Ord, Show)

pQuotedString :: ParserL QuotedString
pQuotedString = QuotedString
  <$> pDoubleQuote
  <*> many pQuotedChar
  <*> pDoubleQuote

rQuotedString :: QuotedString -> ShowS
rQuotedString (QuotedString q0 cs1 q2)
  = rDoubleQuote q0 . rList rQuotedChar cs1 . rDoubleQuote q2

data UnquotedString
  = UnquotedString [D9z] USCharNonDigit [Either USCharNonDigit D9z]
  deriving (Eq, Ord, Show)

pUnquotedString :: ParserL UnquotedString
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

pUnquotedCommodityOnLeft :: ParserL UnquotedCommodityOnLeft
pUnquotedCommodityOnLeft = UnquotedCommodityOnLeft
  <$> pUSCharNonDigit <*> many pUSCharNonDigit

rUnquotedCommodityOnLeft :: UnquotedCommodityOnLeft -> ShowS
rUnquotedCommodityOnLeft (UnquotedCommodityOnLeft c0 ls1)
  = rUSCharNonDigit c0 . rList rUSCharNonDigit ls1

data UnquotedCommodityOnRight
  = UnquotedCommodityOnRight USCharNonDigit [USCharNonDigit]
  deriving (Eq, Ord, Show)

pUnquotedCommodityOnRight :: ParserL UnquotedCommodityOnRight
pUnquotedCommodityOnRight = UnquotedCommodityOnRight
  <$> pUSCharNonDigit
  <*> many pUSCharNonDigit

rUnquotedCommodityOnRight :: UnquotedCommodityOnRight -> ShowS
rUnquotedCommodityOnRight (UnquotedCommodityOnRight d0 ls1)
  = rUSCharNonDigit d0 . rList rUSCharNonDigit ls1

newtype CommodityOnLeftA
  = CommodityOnLeftA (Either UnquotedCommodityOnLeft QuotedCommodity)
  deriving (Eq, Ord, Show)

pCommodityOnLeftA :: ParserL CommodityOnLeftA
pCommodityOnLeftA = CommodityOnLeftA
  <$> pEither pUnquotedCommodityOnLeft pQuotedCommodity

rCommodityOnLeftA :: CommodityOnLeftA -> ShowS
rCommodityOnLeftA (CommodityOnLeftA ei)
  = either rUnquotedCommodityOnLeft rQuotedCommodity ei

newtype CommodityOnRightA
  = CommodityOnRightA (Either UnquotedCommodityOnRight QuotedCommodity)
  deriving (Eq, Ord, Show)

pCommodityOnRightA :: ParserL CommodityOnRightA
pCommodityOnRightA = CommodityOnRightA
  <$> pEither pUnquotedCommodityOnRight pQuotedCommodity

rCommodityOnRightA :: CommodityOnRightA -> ShowS
rCommodityOnRightA (CommodityOnRightA ei)
  = either rUnquotedCommodityOnRight rQuotedCommodity ei

newtype QuotedCommodity = QuotedCommodity QuotedString
  deriving (Eq, Ord, Show)

pQuotedCommodity :: ParserL QuotedCommodity
pQuotedCommodity = QuotedCommodity <$> pQuotedString

rQuotedCommodity :: QuotedCommodity -> ShowS
rQuotedCommodity (QuotedCommodity q0) = rQuotedString q0

newtype CommodityA
  = CommodityA (Either UnquotedCommodityOnRight QuotedCommodity)
  deriving (Eq, Ord, Show)

pCommodityA :: ParserL CommodityA
pCommodityA = CommodityA <$>
  (Left <$> pUnquotedCommodityOnRight <|> Right <$> pQuotedCommodity)

rCommodityA :: CommodityA -> ShowS
rCommodityA (CommodityA ei)
  = either rUnquotedCommodityOnRight rQuotedCommodity ei

data Backtick = Backtick
  deriving (Eq, Ord, Show)

pBacktick :: ParserL Backtick
pBacktick = Backtick <$ pSym '`'

rBacktick :: Backtick -> ShowS
rBacktick Backtick = ('`':)

data NonNeutral
  = NonNeutralRadCom Backtick (Brim RadCom)
  | NonNeutralRadPer (Brim RadPer)
  deriving (Eq, Ord, Show)

pNonNeutral :: ParserL NonNeutral
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

pNeutralOrNon :: ParserL NeutralOrNon
pNeutralOrNon
  = NeutralOrNonRadCom <$> pBacktick <*>
      (Left <$> pNil pRadixRadCom pRadCom
        <|> Right <$> pBrim pRadixRadCom pRadCom)
  <|> NeutralOrNonRadPer <$>
      (Left <$> pNil pRadixRadPer pRadPer
        <|> Right <$> pBrim pRadixRadPer pRadPer)

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

pNeutral :: ParserL Neutral
pNeutral
  = NeuCom <$> pBacktick <*> pNil pRadixRadCom pRadCom
  <|> NeuPer <$> pNil pRadixRadPer pRadPer

rNeutral :: Neutral -> ShowS
rNeutral neu = case neu of
  NeuCom b0 n1 -> rBacktick b0 . rNil rRadixRadCom rRadCom n1
  NeuPer n0 -> rNil rRadixRadPer rRadPer n0

-- | Trio.  There is nothing corresponding to 'Penny.Lincoln.Trio.E'
-- as this would screw up the spacing, and generally productions in
-- the AST should actually produce something.  Instead,
-- 'Penny.Lincoln.Trio.E' is indicated by the absense of any 'TrioA'.
data TrioA
  = QcCyOnLeftA (Fs Side) (Fs CommodityOnLeftA) NonNeutral
  -- ^ Non neutral, commodity on left
  | QcCyOnRightA (Fs Side) (Fs NonNeutral) CommodityOnRightA
  -- ^ Non neutral, commodity on right
  | QSided (Fs Side) NonNeutral
  -- ^ Qty with side only
  | QUnsided Neutral
  -- ^ Qty, with no side
  | SCA (Fs Side) CommodityA
  -- ^ Side and commodity
  | SA Side
  -- ^ Side only
  | UcCyOnLeftA (Fs CommodityOnLeftA) NonNeutral
  -- ^ Unsigned quantity and commodity only, commodity on left
  | UcCyOnRightA (Fs NonNeutral) CommodityOnRightA
  -- ^ Unsigned quantity and commodity only, commodity on right
  | UA NonNeutral
  -- ^ Non-sided non-neutral quantity only
  | CA CommodityA
  -- ^ Commodity only
  deriving (Eq, Ord, Show)

pTrioA :: ParserL TrioA
pTrioA
  = QcCyOnLeftA <$> pFs pSide <*> pFs pCommodityOnLeftA <*> pNonNeutral
  <|> QcCyOnRightA <$> pFs pSide <*> pFs pNonNeutral <*> pCommodityOnRightA
  <|> QSided <$> pFs pSide <*> pNonNeutral
  <|> QUnsided <$> pNeutral
  <|> SCA <$> pFs pSide <*> pCommodityA
  <|> SA <$> pSide
  <|> UcCyOnLeftA <$> pFs pCommodityOnLeftA <*> pNonNeutral
  <|> UcCyOnRightA <$> pFs pNonNeutral <*> pCommodityOnRightA
  <|> UA <$> pNonNeutral
  <|> CA <$> pCommodityA

rTrioA :: TrioA -> ShowS
rTrioA x = case x of
  QcCyOnLeftA s0 c1 n1 -> rFs rSide s0 . rFs rCommodityOnLeftA c1
    . rNonNeutral n1
  QcCyOnRightA s0 n1 c2 -> rFs rSide s0 . rFs rNonNeutral n1
    . rCommodityOnRightA c2
  QSided s0 n1 -> rFs rSide s0 . rNonNeutral n1
  QUnsided n0 -> rNeutral n0
  SCA s0 c1 -> rFs rSide s0 . rCommodityA c1
  SA s -> rSide s
  UcCyOnLeftA c0 n1 -> rFs rCommodityOnLeftA c0 . rNonNeutral n1
  UcCyOnRightA n0 c1 -> rFs rNonNeutral n0 . rCommodityOnRightA c1
  UA n -> rNonNeutral n
  CA c -> rCommodityA c

data OpenSquare = OpenSquare
  deriving (Eq, Ord, Show)

pOpenSquare :: ParserL OpenSquare
pOpenSquare = OpenSquare <$ pSym '['

rOpenSquare :: OpenSquare -> ShowS
rOpenSquare OpenSquare = ('[':)

data CloseSquare = CloseSquare
  deriving (Eq, Ord, Show)

pCloseSquare :: ParserL CloseSquare
pCloseSquare = CloseSquare <$ pSym ']'

rCloseSquare :: CloseSquare -> ShowS
rCloseSquare CloseSquare = (']':)

data IntegerA = IntegerA (Either Zero (Maybe PluMin, D9, [D9z]))
  deriving (Eq, Ord, Show)

pIntegerA :: ParserL IntegerA
pIntegerA = IntegerA <$>
  (Left <$> pZero <|> Right <$> ((,,) <$> optional pPluMin
                                      <*> pD9 <*> many pD9z))

rIntegerA :: IntegerA -> ShowS
rIntegerA (IntegerA ei) = case ei of
  Left z -> rZero z
  Right (m0, n1, ds2) -> rMaybe rPluMin m0 . rD9 n1 . rList rD9z ds2

data ScalarA
  = ScalarUnquotedString UnquotedString
  | ScalarQuotedString QuotedString
  | ScalarDate DateA
  | ScalarTime TimeA
  | ScalarZone ZoneA
  | ScalarInt IntegerA
  deriving (Eq, Ord, Show)

pScalarA :: ParserL ScalarA
pScalarA
  = ScalarDate <$> pDateA
  <|> ScalarTime <$> pTimeA
  <|> ScalarZone <$> pZoneA
  <|> ScalarInt <$> pIntegerA
  <|> ScalarQuotedString <$> pQuotedString
  <|> ScalarUnquotedString <$> pUnquotedString

rScalarA :: ScalarA -> ShowS
rScalarA sclrA = case sclrA of
  ScalarUnquotedString x -> rUnquotedString x
  ScalarQuotedString x -> rQuotedString x
  ScalarDate x -> rDateA x
  ScalarTime x -> rTimeA x
  ScalarZone x -> rZoneA x
  ScalarInt x -> rIntegerA x

data BracketedForest = BracketedForest
  (Fs OpenSquare) (Maybe (Fs ForestA)) CloseSquare
  deriving (Eq, Ord, Show)

data ForestA = ForestA TreeA [(Whites, TreeA)]
  deriving (Eq, Ord, Show)

pForestA :: ParserL ForestA
pForestA = ForestA <$> pTreeA <*> many ((,) <$> pWhites <*> pTreeA)

rForestA :: ForestA -> ShowS
rForestA (ForestA t0 ls1)
  = rTreeA t0 . rList (\(w, t) -> rWhites w . rTreeA t) ls1

pBracketedForest :: ParserL BracketedForest
pBracketedForest = BracketedForest <$> pFs pOpenSquare
  <*> optional (pFs pForestA) <*> pCloseSquare

rBracketedForest :: BracketedForest -> ShowS
rBracketedForest (BracketedForest b0 m1 b2) = rFs rOpenSquare b0
  . rMaybe (rFs rForestA) m1 . rCloseSquare b2

data TreeA = TreeA (Located ScalarA) (Maybe (Bs BracketedForest))
  deriving (Eq, Ord, Show)

pTreeA :: ParserL TreeA
pTreeA = TreeA <$> pLocated pScalarA <*> optional (pBs pBracketedForest)

rTreeA :: TreeA -> ShowS
rTreeA (TreeA s1 m2) = rLocated rScalarA s1
  . rMaybe (rBs rBracketedForest) m2

data TopLineA = TopLineA TreeA [(Whites, TreeA)]
  deriving (Eq, Ord, Show)

pTopLineA :: ParserL TopLineA
pTopLineA = TopLineA <$> pTreeA <*> many ((,) <$> pWhites <*> pTreeA)

rTopLineA :: TopLineA -> ShowS
rTopLineA (TopLineA t0 ts1) = rTreeA t0
  . rList (\(w, t) -> rWhites w . rTreeA t) ts1

data PostingA
  = PostingTrioFirst (Located TrioA) (Maybe (Bs BracketedForest))
  | PostingNoTrio BracketedForest
  deriving (Eq, Ord, Show)

pPostingA :: ParserL PostingA
pPostingA
  = PostingTrioFirst <$> pLocated pTrioA
                     <*> optional (pBs pBracketedForest)
  <|> PostingNoTrio <$> pBracketedForest

rPostingA :: PostingA -> ShowS
rPostingA pstg = case pstg of
  PostingTrioFirst t0 m1 -> rLocated rTrioA t0
    . rMaybe (rBs rBracketedForest) m1
  PostingNoTrio b0 -> rBracketedForest b0

data OpenCurly = OpenCurly
  deriving (Eq, Ord, Show)

pOpenCurly :: ParserL OpenCurly
pOpenCurly = OpenCurly <$ pSym '{'

rOpenCurly :: OpenCurly -> ShowS
rOpenCurly OpenCurly = ('{':)

data CloseCurly = CloseCurly
  deriving (Eq, Ord, Show)

pCloseCurly :: ParserL CloseCurly
pCloseCurly = CloseCurly <$ pSym '}'

rCloseCurly :: CloseCurly -> ShowS
rCloseCurly CloseCurly = ('}':)

data CommaA = CommaA
  deriving (Eq, Ord, Show)

pCommaA :: ParserL CommaA
pCommaA = CommaA <$ pSym ','

rCommaA :: CommaA -> ShowS
rCommaA CommaA = (',':)

data PostingsA = PostingsA (Fs OpenCurly)
  (Maybe (Fs PostingList)) CloseCurly
  deriving (Eq, Ord, Show)

pPostingsA :: ParserL PostingsA
pPostingsA = PostingsA <$> pFs pOpenCurly
  <*> optional (pFs pPostingList) <*> pCloseCurly

rPostingsA :: PostingsA -> ShowS
rPostingsA (PostingsA c0 m1 c2)
  = rFs rOpenCurly c0 . rMaybe (rFs rPostingList) m1
  . rCloseCurly c2

data Semicolon = Semicolon
  deriving (Eq, Ord, Show)

pSemicolon :: ParserL Semicolon
pSemicolon = Semicolon <$ pSym ';'

rSemicolon :: Semicolon -> ShowS
rSemicolon Semicolon = (';':)

data PostingList
  = OnePosting (Located PostingA)
  | PostingList (Located PostingA) (Bs Semicolon) (Bs (Located PostingA))
                [(Bs Semicolon, Bs (Located PostingA))]
  deriving (Eq, Ord, Show)

pPostingList :: ParserL PostingList
pPostingList
  = OnePosting <$> (pLocated pPostingA)
  <|> PostingList <$> pLocated pPostingA <*> pBs pSemicolon
      <*> pBs (pLocated pPostingA)
      <*> many ((,) <$> pBs pSemicolon <*> pBs (pLocated pPostingA))

rPostingList :: PostingList -> ShowS
rPostingList pl = case pl of
  OnePosting p0 -> rLocated rPostingA p0
  PostingList p0 s1 p2 ls3 -> rLocated rPostingA p0
    . rBs rSemicolon s1 . rBs (rLocated rPostingA) p2
    . rList (\(s4, p5) -> rBs rSemicolon s4 . rBs (rLocated rPostingA) p5) ls3

data TransactionA
  = TransactionWithTopLine (Located TopLineA) (Bs PostingsA)
  | TransactionNoTopLine PostingsA
  deriving (Eq, Ord, Show)

pTransactionA :: ParserL TransactionA
pTransactionA
  = TransactionWithTopLine <$> pLocated pTopLineA
      <*> pBs pPostingsA
  <|> TransactionNoTopLine <$> pPostingsA

rTransactionA :: TransactionA -> ShowS
rTransactionA txn = case txn of
  TransactionWithTopLine t0 p1 -> rLocated (rTopLineA) t0
    . rBs rPostingsA p1
  TransactionNoTopLine p0 -> rPostingsA p0

data AtSign = AtSign
  deriving (Eq, Ord, Show)

pAtSign :: ParserL AtSign
pAtSign = AtSign <$ pSym '@'

rAtSign :: AtSign -> ShowS
rAtSign AtSign = ('@':)

data PriceA = PriceA LineColPosA (Fs AtSign) (Located DateA) Whites
  (Maybe (Located (TimeA, Whites))) (Maybe (Located (ZoneA, Whites)))
  CommodityA Whites CyExch
  deriving (Eq, Ord, Show)

pPriceA :: ParserL PriceA
pPriceA = PriceA <$> pPos <*> pFs pAtSign <*> pLocated pDateA
  <*> pWhites
  <*> optional (pLocated ((,) <$> pTimeA <*> pWhites))
  <*> optional (pLocated ((,) <$> pZoneA <*> pWhites))
  <*> pCommodityA <*> pWhites <*> pCyExch

rPriceA :: PriceA -> ShowS
rPriceA (PriceA _ a0 l1 w2 m3 m4 c5 w6 e7)
  = rFs rAtSign a0
  . rLocated rDateA l1
  . rWhites w2
  . rMaybe (rLocated (\(t, w) -> rTimeA t . rWhites w)) m3
  . rMaybe (rLocated (\(z, w) -> rZoneA z . rWhites w)) m4
  . rCommodityA c5
  . rWhites w6
  . rCyExch e7

data ExchA
  = ExchANeutral Neutral
  | ExchANonNeutral (Maybe (Fs PluMin)) NonNeutral
  deriving (Eq, Ord, Show)

pExchA :: ParserL ExchA
pExchA = ExchANeutral <$> pNeutral
  <|> ExchANonNeutral <$> optional (pFs pPluMin) <*> pNonNeutral

rExchA :: ExchA -> ShowS
rExchA exch = case exch of
  ExchANeutral n -> rNeutral n
  ExchANonNeutral mayFs nn -> rMaybe (rFs rPluMin) mayFs . rNonNeutral nn

data CyExch
  = CyExchCy (Fs CommodityA) ExchA
  | CyExchA (Fs ExchA) CommodityA
  deriving (Eq, Ord, Show)

pCyExch :: ParserL CyExch
pCyExch = CyExchCy <$> pFs pCommodityA <*> pExchA
  <|> CyExchA <$> pFs pExchA
               <*> pCommodityA

rCyExch :: CyExch -> ShowS
rCyExch exch = case exch of
  CyExchCy cy0 q1 -> rFs rCommodityA cy0 . rExchA q1
  CyExchA q0 c1 -> rFs rExchA q0 . rCommodityA c1

data FileItem = FileItem (Located (Either PriceA TransactionA))
  deriving (Eq, Ord, Show)

pFileItem :: ParserL FileItem
pFileItem = FileItem
  <$> pLocated ((Left <$> pPriceA) <|> (Right <$> pTransactionA))

rFileItem :: FileItem -> ShowS
rFileItem (FileItem l0) =
  rLocated (either rPriceA rTransactionA) l0

data FileItems = FileItems FileItem [(Whites, FileItem)]
  deriving (Eq, Ord, Show)

pFileItems :: ParserL FileItems
pFileItems = FileItems <$> pFileItem
  <*> many ((,) <$> pWhites <*> pFileItem)

rFileItems :: FileItems -> ShowS
rFileItems (FileItems i0 ls1)
  = rFileItem i0 . rList (\(w, i) -> rWhites w . rFileItem i) ls1

-- | Unlike every other production in this module, 'Ast' may produce
-- an empty input.
data Ast
  = AstNoLeadingWhite (Fs FileItems)
  | AstLeadingWhite Whites (Maybe (Fs FileItems))
  | EmptyFile
  deriving (Eq, Ord, Show)

pAst :: ParserL Ast
pAst
  = AstNoLeadingWhite <$> pFs pFileItems
  <|> AstLeadingWhite <$> pWhites <*> (optional (pFs pFileItems))
  <|> pure EmptyFile

rAst :: Ast -> ShowS
rAst fl = case fl of
  AstNoLeadingWhite f0 -> rFs rFileItems f0
  AstLeadingWhite w0 m1 -> rWhites w0 . rMaybe (rFs rFileItems) m1
  EmptyFile -> id

-- | Parses an entire 'Ast' from a string.
--
-- Returns the parsed 'Ast', any error messages, and any errors
-- resulting because input was not parsed.  This parser is \"online\",
-- so it lazily processes the input 'String'.  Thus it is possible to
-- traverse the result 'Ast' while lazily processing the input
-- 'String'.  Examining either final list of 'Error' will also cause
-- the parse to proceed.  Therefore, to preserve laziness, handle the
-- result 'Ast' first, and then look for errors.
--
-- As this return type suggests, you will always get a result, even
-- if the input contains errors; uu-parsinglib will make insertions
-- or deletions to the text as it parses.  It will take whatever
-- steps are necessary to get a complete 'Ast'.  However such
-- changes will be indicated in the list of 'Error'.  This scheme
-- delivers excellent error messages because instead of stopping an
-- entire parse due to a single error, the parse will proceed to
-- find further errors later in the file.  Of course fixing the
-- first error might fix all the later ones, but at least this is
-- left up to the user.

parseAst
  :: String
  -> (Ast, [Error LineColPosA], [Error LineColPosA])
parseAst str = parse prsr (createStr (LineColPosA 1 0 0) str)
  where
    prsr = (,,) <$> pAst <*> pErrors <*> pEnd

displayParseError :: Error LineColPosA -> String
displayParseError er = case er of
  Inserted i p es -> "inserted " ++ i ++ " at " ++ display p ""
    ++ "; expecting " ++ expecting es
  Deleted i p es -> "deleted " ++ i ++ " at " ++ display p ""
    ++ "; expecting " ++ expecting es
  Replaced _ _ _ _ -> error "replaced: not implemented"
  DeletedAtEnd s -> "unconsumed input deleted: " ++ s
  where
    expecting ls = case ls of
      [] -> "empty list"
      x:[] -> x
      xs -> "one of: " ++ (concat . intersperse "; " $ xs)
