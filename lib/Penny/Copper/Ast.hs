module Penny.Copper.Ast where

import Control.Applicative
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)
import Text.ParserCombinators.UU.Core
import Penny.Lincoln.Rep
import Penny.Copper.Terminals
import Penny.Lincoln.Side
import Penny.Lincoln.PluMin
import Penny.Copper.Parser
import Penny.Copper.LincolnTypes

data Located a = Located LineColPosA a
  deriving (Eq, Ord, Show)

instance Functor Located where
  fmap f (Located l1 a) = Located l1 (f a)

pLocated :: Parser a -> Parser (Located a)
pLocated p = Located <$> pPos <*> p

rLocated :: (a -> ShowS) -> Located a -> ShowS
rLocated f (Located _ a) = f a

-- | Something that might be followed by spaces.
data Fs a = Fs a (Maybe Whites)
  deriving (Eq, Ord, Show)

rFs :: (a -> ShowS) -> Fs a -> ShowS
rFs f (Fs a mw) = f a . rMaybe rWhites mw

instance Functor Fs where
  fmap f (Fs a w) = Fs (f a) w

pFs :: Parser a -> Parser (Fs a)
pFs p = Fs <$> p <*> optional pWhites

-- | Something that might be preceded by spaces.
data Bs a = Bs (Maybe Whites) a
  deriving (Eq, Ord, Show)

instance Functor Bs where
  fmap f (Bs w a) = Bs w (f a)

pBs :: Parser a -> Parser (Bs a)
pBs p = Bs <$> optional pWhites <*> p

rBs :: (a -> ShowS) -> Bs a -> ShowS
rBs f (Bs w a) = rMaybe rWhites w . f a

-- | Octothorpe
data Hash = Hash
  deriving (Eq, Ord, Show)

pHash :: Parser Hash
pHash = Hash <$ pSym '#'

rHash :: Hash -> ShowS
rHash Hash = ('#':)

data Newline = Newline
  deriving (Eq, Ord, Show)

pNewline :: Parser Newline
pNewline = Newline <$ pSym '\n'

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

data DigitsFour = DigitsFour Decem Decem Decem Decem
  deriving (Eq, Ord, Show)

pDigitsFour :: Parser DigitsFour
pDigitsFour = DigitsFour <$> pDecem <*> pDecem <*> pDecem <*> pDecem

rDigitsFour :: DigitsFour -> ShowS
rDigitsFour (DigitsFour d0 d1 d2 d3) = rDecem d0
  . rDecem d1 . rDecem d2 . rDecem d3

data Digits1or2 = Digits1or2 Decem (Maybe Decem)
  deriving (Eq, Ord, Show)

pDigits1or2 :: Parser Digits1or2
pDigits1or2 = Digits1or2 <$> pDecem <*> optional pDecem

rDigits1or2 :: Digits1or2 -> ShowS
rDigits1or2 (Digits1or2 d0 m1) = rDecem d0 . rMaybe rDecem m1

data DateSep = DateSlash | DateHyphen
  deriving (Eq, Ord, Show)

data DateA = DateA
  DigitsFour DateSep Digits1or2 DateSep Digits1or2
  deriving (Eq, Ord, Show)

pDateSep :: Parser DateSep
pDateSep = DateSlash <$ pSym '/' <|> DateHyphen <$ pSym '-'

rDateSep :: DateSep -> ShowS
rDateSep DateSlash = ('/':)
rDateSep DateHyphen = ('-':)

pDateA :: Parser DateA
pDateA = DateA <$> pDigitsFour <*> pDateSep
               <*> pDigits1or2 <*> pDateSep <*> pDigits1or2

rDateA :: DateA -> ShowS
rDateA (DateA d0 s1 d2 s3 d4) = rDigitsFour d0 . rDateSep s1
  . rDigits1or2 d2 . rDateSep s3 . rDigits1or2 d4

data Colon = Colon
  deriving (Eq, Ord, Show)

pColon :: Parser Colon
pColon = Colon <$ pSym ':'

rColon :: Colon -> ShowS
rColon Colon = (':':)

data TimeA = TimeA
  Digits1or2 Colon Digits1or2 (Maybe (Colon, Digits1or2))
  deriving (Eq, Ord, Show)

pTimeA :: Parser TimeA
pTimeA = TimeA <$> pDigits1or2 <*> pColon <*> pDigits1or2
    <*> optional ((,) <$> pColon <*> pDigits1or2)

rTimeA :: TimeA -> ShowS
rTimeA (TimeA d0 c1 d2 m3) = rDigits1or2 d0 . rColon c1
  . rDigits1or2 d2 . rMaybe (\(c3, d4) -> rColon c3 . rDigits1or2 d4) m3

data ZoneA = ZoneA Backtick PluMin DigitsFour
  deriving (Eq, Ord, Show)

rZoneA :: ZoneA -> ShowS
rZoneA (ZoneA b0 p1 d2)
  = rBacktick b0 . rPluMin p1 . rDigitsFour d2

pZoneA :: Parser ZoneA
pZoneA = ZoneA <$> pBacktick <*> pPluMin <*> pDigitsFour

data DoubleQuote = DoubleQuote
  deriving (Eq, Ord, Show)

pDoubleQuote :: Parser DoubleQuote
pDoubleQuote = DoubleQuote <$ pSym '"'

rDoubleQuote :: DoubleQuote -> ShowS
rDoubleQuote DoubleQuote = ('"':)

data Backslash = Backslash
  deriving (Eq, Ord, Show)

pBackslash :: Parser Backslash
pBackslash = Backslash <$ pSym '\\'

rBackslash :: Backslash -> ShowS
rBackslash Backslash = ('\\':)

data White
  = Space
  | Tab
  | WhiteNewline
  | WhiteComment Comment
  deriving (Eq, Ord, Show)

pWhite :: Parser White
pWhite = Space <$ pSym ' ' <|> Tab <$ pSym '\t'
  <|> WhiteNewline <$ pSym '\n'
  <|> WhiteComment <$> pComment

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

data EscPayload
  = EscBackslash
  | EscNewline
  | EscQuote
  | EscGap Whites Backslash
  deriving (Eq, Ord, Show)

pEscPayload :: Parser EscPayload
pEscPayload = EscBackslash <$ pSym '\\'
  <|> EscNewline <$ pSym '\n'
  <|> EscQuote <$ pSym '"'
  <|> EscGap <$> pWhites <*> pBackslash

rEscPayload :: EscPayload -> ShowS
rEscPayload x = case x of
  EscBackslash -> ('\\':)
  EscNewline -> ('\n':)
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
  <*> many pQuotedChar
  <*> pDoubleQuote

rQuotedString :: QuotedString -> ShowS
rQuotedString (QuotedString q0 cs1 q2)
  = rDoubleQuote q0 . rList rQuotedChar cs1 . rDoubleQuote q2

data UnquotedString
  = UnquotedString [USCharOpt] USCharReq [USCharOpt]
  deriving (Eq, Ord, Show)

pUnquotedString :: Parser UnquotedString
pUnquotedString = UnquotedString
  <$> pList pUSCharOpt <*> pUSCharReq <*> pList pUSCharOpt

rUnquotedString :: UnquotedString -> ShowS
rUnquotedString (UnquotedString ls0 c1 ls2) =
  rList rUSCharOpt ls0 . rUSCharReq c1 . rList rUSCharOpt ls2

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
-- can also be a non-quoted string; in this case, it must not begin
-- with a digit (the other characters may be digits.)

data UnquotedCommodity
  = UnquotedCommodity UnquotedCommodityFirstChar [USCharOpt]
  deriving (Eq, Ord, Show)

pUnquotedCommodity :: Parser UnquotedCommodity
pUnquotedCommodity = UnquotedCommodity
  <$> pUnquotedCommodityFirstChar <*> many pUSCharOpt

rUnquotedCommodity :: UnquotedCommodity -> ShowS
rUnquotedCommodity (UnquotedCommodity c0 ls1)
  = rUnquotedCommodityFirstChar c0 . rList rUSCharOpt ls1

newtype QuotedCommodity = QuotedCommodity QuotedString
  deriving (Eq, Ord, Show)

pQuotedCommodity :: Parser QuotedCommodity
pQuotedCommodity = QuotedCommodity <$> pQuotedString

rQuotedCommodity :: QuotedCommodity -> ShowS
rQuotedCommodity (QuotedCommodity q0) = rQuotedString q0

newtype CommodityA
  = CommodityA (Either UnquotedCommodity QuotedCommodity)
  deriving (Eq, Ord, Show)

pCommodityA :: Parser CommodityA
pCommodityA = CommodityA <$>
  (Left <$> pUnquotedCommodity <|> Right <$> pQuotedCommodity)

rCommodityA :: CommodityA -> ShowS
rCommodityA (CommodityA ei) = either rUnquotedCommodity rQuotedCommodity ei

data Backtick = Backtick
  deriving (Eq, Ord, Show)

pBacktick :: Parser Backtick
pBacktick = Backtick <$ pSym '`'

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

-- | Trio.  There is nothing corresponding to 'Penny.Lincoln.Trio.E'
-- as this would screw up the spacing, and generally productions in
-- the AST should actually produce something.  Instead,
-- 'Penny.Lincoln.Trio.E' is indicated by the absense of any 'TrioA'.
data TrioA
  = QcCyOnLeftA (Fs Side) (Fs CommodityA) NonNeutral
  -- ^ Non neutral, commodity on left
  | QcCyOnRightA (Fs Side) (Fs NonNeutral) CommodityA
  -- ^ Non neutral, commodity on right
  | QA (Fs Side) NeutralOrNon
  -- ^ Qty with side only
  | SCA (Fs Side) CommodityA
  -- ^ Side and commodity
  | SA Side
  -- ^ Side only
  | UcCyOnLeftA (Fs CommodityA) NonNeutral
  -- ^ Unsigned quantity and commodity only, commodity on left
  | UcCyOnRightA (Fs NonNeutral) CommodityA
  -- ^ Unsigned quantity and commodity only, commodity on right
  | UA NonNeutral
  -- ^ Non-sided non-neutral quantity only
  | CA CommodityA
  -- ^ Commodity only
  deriving (Eq, Ord, Show)

pTrioA :: Parser TrioA
pTrioA = QcCyOnLeftA <$> pFs pSide <*> pFs pCommodityA <*> pNonNeutral
  <|> QcCyOnRightA <$> pFs pSide <*> pFs pNonNeutral <*> pCommodityA
  <|> QA <$> pFs pSide <*> pNeutralOrNon
  <|> SCA <$> pFs pSide <*> pCommodityA
  <|> SA <$> pSide
  <|> UcCyOnLeftA <$> pFs pCommodityA <*> pNonNeutral
  <|> UcCyOnRightA <$> pFs pNonNeutral <*> pCommodityA
  <|> UA <$> pNonNeutral
  <|> CA <$> pCommodityA

rTrioA :: TrioA -> ShowS
rTrioA x = case x of
  QcCyOnLeftA s0 c1 n1 -> rFs rSide s0 . rFs rCommodityA c1
    . rNonNeutral n1
  QcCyOnRightA s0 n1 c2 -> rFs rSide s0 . rFs rNonNeutral n1
    . rCommodityA c2
  QA s0 n1 -> rFs rSide s0 . rNeutralOrNon n1
  SCA s0 c1 -> rFs rSide s0 . rCommodityA c1
  SA s -> rSide s
  UcCyOnLeftA c0 n1 -> rFs rCommodityA c0 . rNonNeutral n1
  UcCyOnRightA n0 c1 -> rFs rNonNeutral n0 . rCommodityA c1
  UA n -> rNonNeutral n
  CA c -> rCommodityA c

data OpenSquare = OpenSquare
  deriving (Eq, Ord, Show)

pOpenSquare :: Parser OpenSquare
pOpenSquare = OpenSquare <$ pSym '['

rOpenSquare :: OpenSquare -> ShowS
rOpenSquare OpenSquare = ('[':)

data CloseSquare = CloseSquare
  deriving (Eq, Ord, Show)

pCloseSquare :: Parser CloseSquare
pCloseSquare = CloseSquare <$ pSym ']'

rCloseSquare :: CloseSquare -> ShowS
rCloseSquare CloseSquare = (']':)

data IntegerA = IntegerA (Either Zero (Maybe PluMin, Novem, [Decem]))
  deriving (Eq, Ord, Show)

pIntegerA :: Parser IntegerA
pIntegerA = IntegerA <$>
  (Left <$> pZero <|> Right <$> ((,,) <$> optional pPluMin
                                      <*> pNovem <*> many pDecem))

rIntegerA :: IntegerA -> ShowS
rIntegerA (IntegerA ei) = case ei of
  Left z -> rZero z
  Right (m0, n1, ds2) -> rMaybe rPluMin m0 . rNovem n1 . rList rDecem ds2

data ScalarA
  = ScalarUnquotedString UnquotedString
  | ScalarQuotedString QuotedString
  | ScalarDate DateA
  | ScalarTime TimeA
  | ScalarZone ZoneA
  | ScalarInt IntegerA
  deriving (Eq, Ord, Show)

pScalarA :: Parser ScalarA
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
  (Fs OpenSquare) (Fs TreeA) CloseSquare
  deriving (Eq, Ord, Show)

pBracketedForest :: Parser BracketedForest
pBracketedForest = BracketedForest <$> pFs pOpenSquare
  <*> pFs pTreeA <*> pCloseSquare

rBracketedForest :: BracketedForest -> ShowS
rBracketedForest (BracketedForest b0 t1 b2) = rFs rOpenSquare b0
  . rFs rTreeA t1 . rCloseSquare b2

data TreeA = TreeA (Located ScalarA) (Maybe (Bs BracketedForest))
  deriving (Eq, Ord, Show)

pTreeA :: Parser TreeA
pTreeA = TreeA <$> pLocated pScalarA <*> optional (pBs pBracketedForest)

rTreeA :: TreeA -> ShowS
rTreeA (TreeA s1 m2) = rLocated rScalarA s1
  . rMaybe (rBs rBracketedForest) m2

data TopLineA = TopLineA TreeA [Bs TreeA]
  deriving (Eq, Ord, Show)

pTopLineA :: Parser TopLineA
pTopLineA = TopLineA <$> pTreeA <*> many (pBs pTreeA)

rTopLineA :: TopLineA -> ShowS
rTopLineA (TopLineA t0 ts1) = rTreeA t0 . rList (rBs rTreeA) ts1

data PostingA
  = PostingTrioFirst (Located (Fs TrioA)) (Maybe BracketedForest)
  | PostingNoTrio BracketedForest
  deriving (Eq, Ord, Show)

pPostingA :: Parser PostingA
pPostingA
  = PostingTrioFirst <$> pLocated (pFs pTrioA)
                     <*> optional pBracketedForest
  <|> PostingNoTrio <$> pBracketedForest

rPostingA :: PostingA -> ShowS
rPostingA pstg = case pstg of
  PostingTrioFirst t0 m1 -> rLocated (rFs rTrioA) t0
    . rMaybe rBracketedForest m1
  PostingNoTrio b0 -> rBracketedForest b0

data OpenCurly = OpenCurly
  deriving (Eq, Ord, Show)

pOpenCurly :: Parser OpenCurly
pOpenCurly = OpenCurly <$ pSym '{'

rOpenCurly :: OpenCurly -> ShowS
rOpenCurly OpenCurly = ('{':)

data CloseCurly = CloseCurly
  deriving (Eq, Ord, Show)

pCloseCurly :: Parser CloseCurly
pCloseCurly = CloseCurly <$ pSym '}'

rCloseCurly :: CloseCurly -> ShowS
rCloseCurly CloseCurly = ('}':)

data CommaA = CommaA
  deriving (Eq, Ord, Show)

pCommaA :: Parser CommaA
pCommaA = CommaA <$ pSym ','

rCommaA :: CommaA -> ShowS
rCommaA CommaA = (',':)

data PostingsA = PostingsA (Fs OpenCurly)
  (Maybe (Fs PostingList)) CloseCurly
  deriving (Eq, Ord, Show)

pPostingsA :: Parser PostingsA
pPostingsA = PostingsA <$> pFs pOpenCurly
  <*> optional (pFs pPostingList) <*> pCloseCurly

rPostingsA :: PostingsA -> ShowS
rPostingsA (PostingsA c0 m1 c2)
  = rFs rOpenCurly c0 . rMaybe (rFs rPostingList) m1
  . rCloseCurly c2

data PostingList = PostingList (Located (Fs PostingA))
  [(CommaA, Bs (Located PostingA))]
  deriving (Eq, Ord, Show)

pPostingList :: Parser PostingList
pPostingList = PostingList <$> pLocated (pFs pPostingA)
  <*> many ((,) <$> pCommaA <*> pBs (pLocated pPostingA))

rPostingList :: PostingList -> ShowS
rPostingList (PostingList p0 ls1)
  = rLocated (rFs rPostingA) p0
  . rList (\(c2, bs3) -> rCommaA c2 . rBs (rLocated rPostingA) bs3) ls1

data TransactionA
  = TransactionWithTopLine (Located TopLineA)
      (Maybe (Bs PostingsA))
  | TransactionNoTopLine PostingsA
  deriving (Eq, Ord, Show)

pTransactionA :: Parser TransactionA
pTransactionA
  = TransactionWithTopLine <$> pLocated pTopLineA
      <*> optional (pBs pPostingsA)
  <|> TransactionNoTopLine <$> pPostingsA

rTransactionA :: TransactionA -> ShowS
rTransactionA txn = case txn of
  TransactionWithTopLine t0 m1 -> rLocated (rTopLineA) t0
    . rMaybe (rBs rPostingsA) m1
  TransactionNoTopLine p0 -> rPostingsA p0

data AtSign = AtSign
  deriving (Eq, Ord, Show)

pAtSign :: Parser AtSign
pAtSign = AtSign <$ pSym '@'

rAtSign :: AtSign -> ShowS
rAtSign AtSign = ('@':)

data PriceA = PriceA (Fs AtSign) (Located (Fs DateA))
  (Maybe (Located (Fs TimeA))) (Maybe (Located (Fs ZoneA)))
  (Fs CommodityA) ExchA
  deriving (Eq, Ord, Show)

pPriceA :: Parser PriceA
pPriceA = PriceA <$> pFs pAtSign <*> pLocated (pFs pDateA)
  <*> optional (pLocated (pFs pTimeA))
  <*> optional (pLocated (pFs pZoneA))
  <*> pFs pCommodityA <*> pExchA

rPriceA :: PriceA -> ShowS
rPriceA (PriceA a0 l1 m2 m3 fs4 e5)
  = rFs rAtSign a0
  . rLocated (rFs rDateA) l1
  . rMaybe (rLocated (rFs rTimeA)) m2
  . rMaybe (rLocated (rFs rZoneA)) m3
  . rFs rCommodityA fs4
  . rExchA e5

data ExchA
  = ExchACy (Fs CommodityA) (Maybe (Fs PluMin)) NeutralOrNon
  | ExchAQty (Maybe (Fs PluMin)) (Fs NeutralOrNon) CommodityA
  deriving (Eq, Ord, Show)

pExchA :: Parser ExchA
pExchA = ExchACy <$> pFs pCommodityA <*> optional (pFs pPluMin)
               <*> pNeutralOrNon
  <|> ExchAQty <$> optional (pFs pPluMin) <*> pFs pNeutralOrNon
               <*> pCommodityA

rExchA :: ExchA -> ShowS
rExchA exch = case exch of
  ExchACy fs0 m1 n2 -> rFs rCommodityA fs0
    . rMaybe (rFs rPluMin) m1 . rNeutralOrNon n2
  ExchAQty m0 fs1 c2 -> rMaybe (rFs rPluMin) m0 . rFs rNeutralOrNon fs1
    . rCommodityA c2

data FileItem = FileItem (Located (Either PriceA TransactionA))
  deriving (Eq, Ord, Show)

pFileItem :: Parser FileItem
pFileItem = FileItem
  <$> pLocated ((Left <$> pPriceA) <|> (Right <$> pTransactionA))

rFileItem :: FileItem -> ShowS
rFileItem (FileItem l0) =
  rLocated (either rPriceA rTransactionA) l0

data FileItems = FileItems FileItem [Bs FileItem]
  deriving (Eq, Ord, Show)

pFileItems :: Parser FileItems
pFileItems = FileItems <$> pFileItem <*> many (pBs pFileItem)

rFileItems :: FileItems -> ShowS
rFileItems (FileItems i0 ls1)
  = rFileItem i0 . rList (rBs rFileItem) ls1

data File
  = FileNoLeadingWhite (Fs FileItems)
  | FileLeadingWhite Whites (Maybe (Fs FileItems))
  deriving (Eq, Ord, Show)

pFile :: Parser File
pFile
  = FileNoLeadingWhite <$> pFs pFileItems
  <|> FileLeadingWhite <$> pWhites <*> (optional (pFs pFileItems))

rFile :: File -> ShowS
rFile fl = case fl of
  FileNoLeadingWhite f0 -> rFs rFileItems f0
  FileLeadingWhite w0 m1 -> rWhites w0 . rMaybe (rFs rFileItems) m1
