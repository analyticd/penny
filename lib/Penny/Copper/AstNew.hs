{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Penny.Copper.AstNew where

import Data.Sequence (Seq)

import Penny.Copper.Terminals
import qualified Penny.DateTime as DateTime
import Penny.Digit
import Penny.PluMin
import Penny.Polar
import Penny.Representation (Brim, Nil, RadCom, RadPer)
import Prelude
  (Eq, Ord, Show, Traversable, Functor, Foldable, Maybe,
   Either)

-- | Octothorpe
data Hash = Hash
  deriving (Eq, Ord, Show)

data Newline = Newline
  deriving (Eq, Ord, Show)

data Comment = Comment Hash (Seq CommentChar) Newline
  deriving (Eq, Ord, Show)

data White
  = Space
  | Tab
  | WhiteNewline
  | WhiteComment Comment
  deriving (Eq, Ord, Show)

data Whites = Whites White (Seq White)
  deriving (Eq, Ord, Show)

data Fs a = Fs a (Maybe Whites)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Bs a = Bs (Maybe Whites) a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data DigitsFour = DigitsFour D9z D9z D9z D9z
  deriving (Eq, Ord, Show)

data Digits1or2 = Digits1or2 D9z (Maybe D9z)
  deriving (Eq, Ord, Show)

data Colon = Colon
  deriving (Eq, Ord, Show)

data Hours
  = H0to19 (Maybe D1z) D9z
  | H20to23 Two D3z
  deriving (Eq, Ord, Show)

data Time = Time Hours Colon DateTime.Minutes
  (Maybe (Colon, DateTime.Seconds))
  deriving (Eq, Ord, Show)

data Backtick = Backtick
  deriving (Eq, Ord, Show)

data ZoneA = ZoneA Backtick DateTime.Zone
  deriving (Eq, Ord, Show)

data DoubleQuote = DoubleQuote
  deriving (Eq, Ord, Show)

data Backslash = Backslash
  deriving (Eq, Ord, Show)

data EscPayload
  = EscBackslash
  | EscNewline
  | EscQuote
  | EscGap Whites Backslash
  deriving (Eq, Ord, Show)

data EscSeq = EscSeq Backslash EscPayload
  deriving (Eq, Ord, Show)

newtype QuotedChar = QuotedChar (Either NonEscapedChar EscSeq)
  deriving (Eq, Ord, Show)

data QuotedString = QuotedString DoubleQuote (Seq QuotedChar) DoubleQuote
  deriving (Eq, Ord, Show)

data UnquotedString
  = UnquotedString (Seq D9z) USCharNonDigit (Seq (Either USCharNonDigit D9z))
  deriving (Eq, Ord, Show)

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

data UnquotedCommodity
  = UnquotedCommodity USCharNonDigit (Seq USCharNonDigit)
  deriving (Eq, Ord, Show)

newtype Commodity
  = Commodity (Either UnquotedCommodity QuotedString)
  deriving (Eq, Ord, Show)

--- Start here

data NonNeutral
  = NonNeutralRadCom Backtick (Brim RadCom)
  | NonNeutralRadPer (Brim RadPer)
  deriving (Eq, Ord, Show)

data NeutralOrNon
  = NeutralOrNonRadCom Backtick (Either (Nil RadCom) (Brim RadCom))
  | NeutralOrNonRadPer (Either (Nil RadPer) (Brim RadPer))
  deriving (Eq, Ord, Show)

data Neutral
  = NeuCom Backtick (Nil RadCom)
  | NeuPer (Nil RadPer)
  deriving (Eq, Ord, Show)

data Trio
  = QcCyOnLeft (Fs Pole) (Fs Commodity) NonNeutral
  -- ^ Non neutral, commodity on left
  | QcCyOnRight (Fs Pole) (Fs NonNeutral) Commodity
  -- ^ Non neutral, commodity on right
  | QSided (Fs Pole) NonNeutral
  -- ^ Qty with side only
  | QUnsided Neutral
  -- ^ Qty, with no side
  | SC (Fs Pole) Commodity
  -- ^ Side and commodity
  | S Pole
  -- ^ Side only
  | UcCyOnLeft (Fs Commodity) NonNeutral
  -- ^ Unsigned quantity and commodity only, commodity on left
  | UcCyOnRight (Fs NonNeutral) Commodity
  -- ^ Unsigned quantity and commodity only, commodity on right
  | U NonNeutral
  -- ^ Non-sided non-neutral quantity only
  | C Commodity
  -- ^ Commodity only
  deriving (Eq, Ord, Show)

data OpenSquare = OpenSquare
  deriving (Eq, Ord, Show)

data CloseSquare = CloseSquare
  deriving (Eq, Ord, Show)

data Integer = Integer (Either Zero (Maybe PluMin, D9, [D9z]))
  deriving (Eq, Ord, Show)

data Scalar
  = ScalarUnquotedString UnquotedString
  | ScalarQuotedString QuotedString
  | ScalarDate DateTime.Date
  | ScalarTime Time
  | ScalarZone ZoneA
  | ScalarInt Integer
  deriving (Eq, Ord, Show)

data BracketedForest = BracketedForest
  (Fs OpenSquare) (Maybe (Fs Forest)) CloseSquare
  deriving (Eq, Ord, Show)

data Comma = Comma
  deriving (Eq, Ord, Show)

data Forest = Forest Tree [(Bs Comma, Bs Tree)]
  deriving (Eq, Ord, Show)

data Tree
  = TreeScalarFirst Scalar (Maybe (Bs BracketedForest))
  | TreeForestFirst BracketedForest (Maybe (Bs Scalar))
  deriving (Eq, Ord, Show)

data OpenCurly = OpenCurly
  deriving (Eq, Ord, Show)

data CloseCurly = CloseCurly
  deriving (Eq, Ord, Show)

data Posting
  = PostingTrioFirst Trio (Maybe (Bs BracketedForest))
  | PostingNoTrio BracketedForest
  deriving (Eq, Ord, Show)

data Semicolon = Semicolon
  deriving (Eq, Ord, Show)

data PostingList
  = OnePosting Posting
  | PostingList Posting (Bs Semicolon) (Bs Posting)
                [(Bs Semicolon, Bs Posting)]
  deriving (Eq, Ord, Show)

data Postings = Postings (Fs OpenCurly)
  (Maybe (Fs PostingList)) CloseCurly
  deriving (Eq, Ord, Show)

data Transaction = Transaction (Maybe (Fs Tree)) Postings
  deriving (Eq, Ord, Show)

data AtSign = AtSign
  deriving (Eq, Ord, Show)

data Exch
  = ExchANeutral Neutral
  | ExchANonNeutral (Maybe (Fs PluMin)) NonNeutral
  deriving (Eq, Ord, Show)

data CyExch
  = CyExchCy (Fs Commodity) Exch
  | CyExchA (Fs Exch) Commodity
  deriving (Eq, Ord, Show)

data Price = Price (Fs AtSign) DateTime.Date Whites
  (Maybe (Time, Whites)) (Maybe (ZoneA, Whites))
  Commodity Whites CyExch
  deriving (Eq, Ord, Show)

data FileItem = FileItem (Either Price Transaction)
  deriving (Eq, Ord, Show)

data FileItems = FileItems FileItem [(Whites, FileItem)]
  deriving (Eq, Ord, Show)

-- | Unlike every other production in this module, 'Ast' may produce
-- an empty input.
data Ast
  = AstNoLeadingWhite (Fs FileItems)
  | AstLeadingWhite Whites (Maybe (Fs FileItems))
  | EmptyFile
  deriving (Eq, Ord, Show)
