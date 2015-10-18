{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Penny.Grammar where

import Data.Sequence (Seq)

import Penny.Polar (Moderated)
import Prelude
  (Eq, Ord, Show, Traversable, Functor, Foldable, Maybe,
   Either, Char)

-- Terminals

type CommentChar = Char

type NonEscapedChar = Char

type USCharNonDigit = Char

-- Digits

data Zero = Zero
  deriving (Eq, Ord, Show)

data One = One
  deriving (Eq, Ord, Show)

data Two = Two
  deriving (Eq, Ord, Show)

data Three = Three
  deriving (Eq, Ord, Show)

data Four = Four
  deriving (Eq, Ord, Show)

data Five = Five
  deriving (Eq, Ord, Show)

data Six = Six
  deriving (Eq, Ord, Show)

data Seven = Seven
  deriving (Eq, Ord, Show)

data Eight = Eight
  deriving (Eq, Ord, Show)

data Nine = Nine
  deriving (Eq, Ord, Show)

data D1z = D1z'0 | D1z'1
  deriving (Eq, Ord, Show)

data D2z = D2z'0 | D2z'1 | D2z'2
  deriving (Eq, Ord, Show)

data D2 = D2'1 | D2'2
  deriving (Eq, Ord, Show)

data D3z = D3z'0 | D3z'1 | D3z'2 | D3z'3
  deriving (Eq, Ord, Show)

data D3 = D3'1 | D3'2 | D3'3
  deriving (Eq, Ord, Show)

data D4z = D4z'0 | D4z'1 | D4z'2 | D4z'3 | D4z'4
  deriving (Eq, Ord, Show)

data D4 = D4'1 | D4'2 | D4'3 | D4'4
  deriving (Eq, Ord, Show)

data D5z = D5z'0 | D5z'1 | D5z'2 | D5z'3 | D5z'4 | D5z'5
  deriving (Eq, Ord, Show)

data D5 = D5'1 | D5'2 | D5'3 | D5'4 | D5'5
  deriving (Eq, Ord, Show)

data D6z = D6z'0 | D6z'1 | D6z'2 | D6z'3 | D6z'4 | D6z'5 | D6z'6
  deriving (Eq, Ord, Show)

data D6 = D6'1 | D6'2 | D6'3 | D6'4 | D6'5 | D6'6
  deriving (Eq, Ord, Show)

data D7z = D7z'0 | D7z'1 | D7z'2 | D7z'3 | D7z'4 | D7z'5 | D7z'6 | D7z'7
  deriving (Eq, Ord, Show)

data D7 = D7'1 | D7'2 | D7'3 | D7'4 | D7'5 | D7'6 | D7'7
  deriving (Eq, Ord, Show)

data D8z = D8z'0 | D8z'1 | D8z'2 | D8z'3 | D8z'4 | D8z'5 | D8z'6 | D8z'7 | D8z'8
  deriving (Eq, Ord, Show)

data D8 = D8'1 | D8'2 | D8'3 | D8'4 | D8'5 | D8'6 | D8'7 | D8'8
  deriving (Eq, Ord, Show)

data D9z = D9z'0 | D9z'1 | D9z'2 | D9z'3 | D9z'4 | D9z'5 | D9z'6 | D9z'7 | D9z'8 | D9z'9
  deriving (Eq, Ord, Show)

data D9 = D9'1 | D9'2 | D9'3 | D9'4 | D9'5 | D9'6 | D9'7 | D9'8 | D9'9
  deriving (Eq, Ord, Show)

data ZeroTo59 = ZeroTo59 (Maybe D5z) D9z
  deriving (Eq, Ord, Show)

--

data PluMin = Plus | Minus
  deriving (Eq, Ord, Show)

--

data DateSep = DateSlash | DateHyphen
  deriving (Eq, Ord, Show)

data Days28
  = D28'1to9 Zero D9
  | D28'10to19 One D9z
  | D28'20to28 Two D8z
  deriving (Eq, Ord, Show)

data Days30
  = D30'28 Days28
  | D30'29 Two Nine
  | D30'30 Three Zero
  deriving (Eq, Ord, Show)

data Days31
  = D31'30 Days30
  | D31'31 Three One
  deriving (Eq, Ord, Show)

data MonthDay
  = Jan Zero One       DateSep Days31
  | Feb Zero Two       DateSep Days28
  | Mar Zero Three     DateSep Days31
  | Apr Zero Four      DateSep Days30
  | May Zero Five      DateSep Days31
  | Jun Zero Six       DateSep Days30
  | Jul Zero Seven     DateSep Days31
  | Aug Zero Eight     DateSep Days31
  | Sep Zero Nine      DateSep Days30
  | Oct One Zero       DateSep Days31
  | Nov One One       DateSep Days30
  | Dec One Two       DateSep Days31
  deriving (Eq, Ord, Show)

data Year = Year D9z D9z D9z D9z
  deriving (Eq, Ord, Show)

data NonLeapDay = NonLeapDay Year DateSep MonthDay
  deriving (Eq, Ord, Show)

data Mod4
  = L00 Zero Zero
  | L04 Zero Four
  | L08 Zero Eight
  | L12 One Two
  | L16 One Six
  | L20 Two Zero
  | L24 Two Four
  | L28 Two Eight
  | L32 Three Two
  | L36 Three Six
  | L40 Four Zero
  | L44 Four Four
  | L48 Four Eight
  | L52 Five Two
  | L56 Five Six
  | L60 Six Zero
  | L64 Six Four
  | L68 Six Eight
  | L72 Seven Two
  | L76 Seven Six
  | L80 Eight Zero
  | L84 Eight Four
  | L88 Eight Eight
  | L92 Nine Two
  | L96 Nine Six
  deriving (Eq, Ord, Show)

data CenturyLeapYear
  = CenturyLeapYear Mod4 Zero Zero
  deriving (Eq, Ord, Show)

data NonCenturyLeapYear
  = NonCenturyLeapYear D9z D9z Mod4
  deriving (Eq, Ord, Show)

data LeapDay = LeapDay
  (Either CenturyLeapYear NonCenturyLeapYear)
  DateSep
  Zero Two
  DateSep
  Two Nine
  deriving (Eq, Ord, Show)

newtype Date = Date (Either NonLeapDay LeapDay)
  deriving (Eq, Ord, Show)

--

-- | A radix point.  The type is parameterized on a type that
-- represents the character used for the radix point.

data Radix a = Radix
  deriving (Eq, Ord, Show)

data Grouper
  = ThinSpace
  | Underscore
  deriving (Eq, Ord, Show)

-- | A radix point of a comma.  This type serves two purposes: when
-- used as a type parameter for a 'Radix', it represents that the
-- radix point is a comma.  When used alone, it represents a grouping
-- character, which may be a period or other grouping character.
data RadCom
  = RCPeriod
  -- ^ When used as a grouping character, a RadCom can be a period
  | RCGrouper Grouper
  -- ^ When used as a grouping character, a RadCom can also be a
  -- 'ThinSpace' or an 'Underscore'.
  deriving (Eq, Ord, Show)

-- | A radix point of a period.  This type serves two purposes: when
-- used as a type parameter for a 'Radix', it represents that the
-- radix point is a period.  When used alone, it represents a grouping
-- character, which may be a comma or other grouping character.
-- | A radix point of a period.
data RadPer
  = RPComma
  -- ^ When used as a grouping character, a RadPer can be a comma
  | RPGrouper Grouper
  -- ^ When used as a grouping character, a RadPer can also be a
  -- 'ThinSpace' or an 'Underscore'.
  deriving (Eq, Ord, Show)

data Nil r
  = NilU (NilUngrouped r)
  | NilG (NilGrouped r)
  deriving (Eq, Ord, Show)

data NilGrouped r
  = NilGrouped (Maybe Zero) (Radix r)
               Zero (Seq Zero) r Zero (Seq Zero)
               (Seq (r, Zero, Seq Zero))
  deriving (Eq, Ord, Show)

data NilUngrouped r
  = NUZero Zero (Maybe (Radix r, Maybe (Zero, Seq Zero)))
  | NURadix (Radix r) Zero (Seq Zero)
  deriving (Eq, Ord, Show)

data Brim r
  = BrimGrouped (BrimGrouped r)
  | BrimUngrouped (BrimUngrouped r)
  deriving (Eq, Ord, Show)

data BrimUngrouped r
  = BUGreaterThanOne D9 (Seq D9z) (Maybe (Radix r, Seq D9z))
  | BULessThanOne (Maybe Zero) (Radix r) (Seq Zero) D9 (Seq D9z)
  deriving (Eq, Ord, Show)

data BrimGrouped r
  = BGGreaterThanOne D9 (Seq D9z) (BG1 r)
  | BGLessThanOne (Maybe Zero) (Radix r) (BG5 r)
  deriving (Eq, Ord, Show)

data BG1 r
  = BG1GroupOnLeft r D9z (Seq D9z) (Seq (r, D9z, Seq D9z))
      (Maybe (Radix r, Maybe (D9z, Seq D9z, Seq (r, D9z, Seq D9z))))
  | BG1GroupOnRight (Radix r) D9z (Seq D9z) r D9z (Seq D9z)
                    (Seq (r, D9z, Seq D9z))
  deriving (Eq, Ord, Show)

data BG5 r
  = BG5Novem D9 (Seq D9z) r D9z (Seq D9z)
                   (Seq (r, D9z, Seq D9z))
  | BG5Zero Zero (Seq Zero) (BG6 r)
  deriving (Eq, Ord, Show)

data BG6 r
  = BG6Novem D9 (Seq D9z) r D9z (Seq D9z)
             (Seq (r, D9z, Seq D9z))
  | BG6Group r (BG7 r)
  deriving (Eq, Ord, Show)

data BG7 r
  = BG7Zeroes Zero (Seq Zero) (BG8 r)
  | BG7Novem D9 (Seq D9z) (Seq (r, D9z, Seq D9z))
  deriving (Eq, Ord, Show)

data BG8 r
  = BG8Novem D9 (Seq D9z) (Seq (r, D9z, Seq D9z))
  | BG8Group r (BG7 r)
  deriving (Eq, Ord, Show)

-- | Number representations that may be neutral or non-neutral.  The
-- type variable is the type of the radix point and grouping
-- character.  Unlike 'NilOrBrimPolar', a 'NilOrBrimScalar' does not
-- have a polarity.

type NilOrBrimScalar r = Either (Nil r) (Brim r)

-- | Number types that may be neutral or non-neutral, with either a
-- comma or period radix.  Does not have a polarity.
type NilOrBrimScalarAnyRadix
  = Either (NilOrBrimScalar RadCom)
           (NilOrBrimScalar RadPer)

type NilScalarAnyRadix = Either (Nil RadCom) (Nil RadPer)
type BrimScalarAnyRadix = Either (Brim RadCom) (Brim RadPer)

-- # Qty representations

-- | Qty representations that may be neutral or non-neutral.  The type
-- variable is the type of the radix point and grouping character;
-- see, for example, 'RadCom' or 'RadPer'.  If non-neutral, also
-- contains a 'Side'.
--
-- This is a complete representation of a quantity; that is, it can
-- represent any quantity.

type Rep r = Moderated (Nil r) (Brim r)

-- | Qty representations that may be neutral or non-neutral and have a
-- radix that is either a period or a comma.  If non-neutral, also
-- contains a 'Side'.

type RepAnyRadix = Either (Rep RadCom) (Rep RadPer)


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

data Time = Time Hours Colon ZeroTo59
  (Maybe (Colon, ZeroTo59))
  deriving (Eq, Ord, Show)

data Backtick = Backtick
  deriving (Eq, Ord, Show)

data Zone = Zone PluMin D2z D3z D9z D9z
  deriving (Eq, Ord, Show)

data QuotedZone = QuotedZone Backtick Zone
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

data LessThan = LessThan
  deriving (Eq, Ord, Show)

data GreaterThan = GreaterThan
  deriving (Eq, Ord, Show)

type Dipole = Either LessThan GreaterThan

data Trio
  = QcCyOnLeft (Fs Dipole) (Fs Commodity) NonNeutral
  -- ^ Non neutral, commodity on left
  | QcCyOnRight (Fs Dipole) (Fs NonNeutral) Commodity
  -- ^ Non neutral, commodity on right
  | QSided (Fs Dipole) NonNeutral
  -- ^ Qty with side only
  | QUnsided Neutral
  -- ^ Qty, with no side
  | SC (Fs Dipole) Commodity
  -- ^ Side and commodity
  | S Dipole
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
  | ScalarDate Date
  | ScalarTime Time
  | ScalarZone QuotedZone
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

data Price = Price (Fs AtSign) Date Whites
  (Maybe (Time, Whites)) (Maybe (QuotedZone, Whites))
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
