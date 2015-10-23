{-# LANGUAGE TemplateHaskell #-}
module Penny.Grammar.Length where

import Control.Lens (makeLenses, over, set)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Sums (caseS4, caseS6)
import Penny.Grammar
import Prelude
  (Eq, Ord, Show, (.), succ, Int, Maybe(Nothing, Just),
   id, either, foldr)

data Position = Position
  { _position :: !Int
  , _line :: !Int
  , _column :: !Int
  } deriving (Eq, Ord, Show)

makeLenses ''Position

type Counter a = a -> [Position -> Position] -> [Position -> Position]

maybe :: Counter a -> Counter (Maybe a)
maybe ctr may = case may of
  Nothing -> id
  Just x -> ctr x

nextChar :: Counter a
nextChar _ = (over column succ . over position succ :)

-- None of the terminals may be a newline, so each of them
-- increments the position, line, and column by one.

commentChar :: Counter CommentChar
commentChar = nextChar

nonEscapedChar :: Counter NonEscapedChar
nonEscapedChar = nextChar

usCharNonDigit :: Counter USCharNonDigit
usCharNonDigit = nextChar

zero :: Counter Zero
zero = nextChar

one :: Counter One
one = nextChar

two :: Counter Two
two = nextChar

three :: Counter Three
three = nextChar

four :: Counter Four
four = nextChar

five :: Counter Five
five = nextChar

six :: Counter Six
six = nextChar

seven :: Counter Seven
seven = nextChar

eight :: Counter Eight
eight = nextChar

nine :: Counter Nine
nine = nextChar

d1z :: Counter D1z
d1z = nextChar

d2z :: Counter D2z
d2z = nextChar

d2 :: Counter D2
d2 = nextChar

d3z :: Counter D3z
d3z = nextChar

d3 :: Counter D3
d3 = nextChar

d4z :: Counter D4z
d4z = nextChar

d4 :: Counter D4
d4 = nextChar

d5z :: Counter D5z
d5z = nextChar

d5 :: Counter D5
d5 = nextChar

d6z :: Counter D6z
d6z = nextChar

d6 :: Counter D6
d6 = nextChar

d7z :: Counter D7z
d7z = nextChar

d7 :: Counter D7
d7 = nextChar

d8z :: Counter D8z
d8z = nextChar

d8 :: Counter D8
d8 = nextChar

d9z :: Counter D9z
d9z = nextChar

d9 :: Counter D9
d9 = nextChar

zeroTo59 :: Counter ZeroTo59
zeroTo59 (ZeroTo59 mayd5 d9) = maybe d5z mayd5 . d9z d9

pluMin :: Counter PluMin
pluMin = nextChar

dateSep :: Counter DateSep
dateSep = nextChar

days28 :: Counter Days28
days28 (D28'1to9 z d) = zero z . d9 d
days28 (D28'10to19 o d) = one o . d9z d
days28 (D28'20to28 t d) = two t . d8z d

days30 :: Counter Days30
days30 x = case x of
  D30'28 a -> days28 a
  D30'29 t n -> two t . nine n
  D30'30 t z -> three t . zero z

days31 :: Counter Days31
days31 x = case x of
  D31'30 d -> days30 d
  D31'31 t o -> three t . one o

monthDay :: Counter MonthDay
monthDay x = case x of
  Jan z o s d -> zero z . one o . dateSep s . days31 d
  Feb z t s d -> zero z . two t . dateSep s . days28 d
  Mar z t s d -> zero z . three t . dateSep s . days31 d
  Apr z f s d -> zero z . four f . dateSep s . days30 d
  May z f s d -> zero z . five f . dateSep s . days31 d
  Jun z x s d -> zero z . six x . dateSep s . days30 d
  Jul z v s d -> zero z . seven v . dateSep s . days31 d
  Aug z e s d -> zero z . eight e . dateSep s . days31 d
  Sep z n s d -> zero z . nine n . dateSep s . days30 d
  Oct o z s d -> one o . zero z . dateSep s . days31 d
  Nov o n s d -> one o . one n . dateSep s . days30 d
  Dec o t s d -> one o . two t . dateSep s . days31 d

year :: Counter Year
year (Year a b c d) = d9z a . d9z b . d9z c . d9z d

nonLeapDay :: Counter NonLeapDay
nonLeapDay (NonLeapDay y s m) = year y . dateSep s . monthDay m

mod4 :: Counter Mod4
mod4 x = case x of
  L00 z e -> zero z . zero e
  L04 z f -> zero z . four f
  L08 z e -> zero z . eight e
  L12 o t -> one o . two t
  L16 o s -> one o . six s
  L20 t z -> two t . zero z
  L24 t f -> two t . four f
  L28 t e -> two t . eight e
  L32 r t -> three r . two t
  L36 t s -> three t . six s
  L40 f z -> four f . zero z
  L44 f o -> four f . four o
  L48 f e -> four f . eight e
  L52 f t -> five f . two t
  L56 f x -> five f . six x
  L60 s z -> six s . zero z
  L64 x f -> six x . four f
  L68 s e -> six s . eight e
  L72 s t -> seven s . two t
  L76 s x -> seven s . six x
  L80 e z -> eight e . zero z
  L84 e f -> eight e . four f
  L88 e i -> eight e . eight i
  L92 n t -> nine n . two t
  L96 n x -> nine n . six x

centuryLeapYear :: Counter CenturyLeapYear
centuryLeapYear (CenturyLeapYear a b c) = mod4 a . zero b . zero c

nonCenturyLeapYear :: Counter NonCenturyLeapYear
nonCenturyLeapYear (NonCenturyLeapYear a b c) = d9z a . d9z b . mod4 c

leapDay :: Counter LeapDay
leapDay (LeapDay a b c d e f g) = either centuryLeapYear nonCenturyLeapYear a
  . dateSep b . zero c . two d . dateSep e . two f . nine g

date :: Counter Date
date (Date ei) = either nonLeapDay leapDay ei

-- Numbers

radixRadCom :: Counter (Radix RadCom)
radixRadCom = nextChar

radixRadPer :: Counter (Radix RadPer)
radixRadPer = nextChar

grouper :: Counter Grouper
grouper = nextChar

radCom :: Counter RadCom
radCom a = case a of
  RCPeriod -> (over column succ . over position succ :)
  RCGrouper g -> grouper g

radPer :: Counter RadPer
radPer a = case a of
  RPComma -> (over column succ . over position succ :)
  RPGrouper g -> grouper g

sequence :: Counter a -> Counter (Seq a)
sequence ctr sq inp = foldr ctr inp (toList sq)

nilGrouped :: Counter (Radix r) -> Counter r -> Counter (NilGrouped r)
nilGrouped rdx r (NilGrouped a b c d e f g h)
  = maybe zero a . rdx b . zero c . sequence zero d . r e . zero f .
  sequence zero g . sequence (\(a, b, c) -> r a . zero b . sequence zero c) h

nilUngrouped :: Counter (Radix r) -> Counter (NilUngrouped r)
nilUngrouped rdx x = case x of
  NUZero a m -> zero a . maybe (\(r, may) -> rdx r . maybe
    (\(z, zs) -> zero z . sequence zero zs) may) m
  NURadix r z zs -> rdx r . zero z . sequence zero zs

nil :: Counter (Radix r) -> Counter r -> Counter (Nil r)
nil rdx g x = case x of
  NilU nu -> nilUngrouped rdx nu
  NilG ng -> nilGrouped rdx g ng

seqDecs :: Counter r -> Counter (Seq (r, D9z, Seq D9z))
seqDecs g = sequence (\(a, b, c) -> g a . d9z b . sequence d9z c)

bg7 :: Counter r -> Counter (BG7 r)
bg7 g x = case x of
  BG7Zeroes z zs b8 -> zero z . sequence zero zs . bg8 g b8
  BG7Novem d sd ss -> d9 d . sequence d9z sd . seqDecs g ss

bg8 :: Counter r -> Counter (BG8 r)
bg8 g x = case x of
  BG8Novem d sd ss -> d9 d . sequence d9z sd . seqDecs g ss
  BG8Group r b7 -> g r . bg7 g b7

bg6 :: Counter r -> Counter (BG6 r)
bg6 g x = case x of
  BG6Novem a b c d e f -> d9 a . sequence d9z b . g c . d9z d
    . sequence d9z e . seqDecs g f
  BG6Group r b7 -> g r . bg7 g b7

bg5 :: Counter r -> Counter (BG5 r)
bg5 g x = case x of
  BG5Novem a b c d e f -> d9 a . sequence d9z b . g c . d9z d
    . sequence d9z e . seqDecs g f
  BG5Zero a b c -> zero a . sequence zero b . bg6 g c

bg1 :: Counter (Radix r) -> Counter r -> Counter (BG1 r)
bg1 r g x = case x of
  BG1GroupOnLeft a b c d e -> g a . d9z b . sequence d9z c . seqDecs g d
    . maybe (\(a, b) -> r a . maybe
        (\(a, b, c) -> d9z a . sequence d9z b . seqDecs g c) b) e
  BG1GroupOnRight a b c d e f h -> r a . d9z b . sequence d9z c . g d
    . d9z e . sequence d9z f . seqDecs g h

brimGrouped :: Counter (Radix r) -> Counter r -> Counter (BrimGrouped r)
brimGrouped r g x = case x of
  BGGreaterThanOne a b c -> d9 a . sequence d9z b . bg1 r g c
  BGLessThanOne a b c -> maybe zero a . r b . bg5 g c

brimUngrouped :: Counter (Radix r) -> Counter (BrimUngrouped r)
brimUngrouped r x = case x of
  BUGreaterThanOne a b c -> d9 a . sequence d9z b .
    maybe (\(a, b) -> r a . sequence d9z b) c
  BULessThanOne a b c d e -> maybe zero a . r b . sequence zero c
    . d9 d . sequence d9z e

brim :: Counter (Radix r) -> Counter r -> Counter (Brim r)
brim r g x = case x of
  BrimGrouped a -> brimGrouped r g a
  BrimUngrouped a -> brimUngrouped r a

nilOrBrimScalar
  :: Counter (Radix r)
  -> Counter r
  -> Counter (NilOrBrimScalar r)
nilOrBrimScalar r g = either (nil r g) (brim r g)

nilOrBrimScalarAnyRadix :: Counter NilOrBrimScalarAnyRadix
nilOrBrimScalarAnyRadix
  = either (nilOrBrimScalar radixRadCom radCom)
           (nilOrBrimScalar radixRadPer radPer)

nilScalarAnyRadix :: Counter NilScalarAnyRadix
nilScalarAnyRadix
  = either (nil radixRadCom radCom) (nil radixRadPer radPer)

brimScalarAnyRadix :: Counter BrimScalarAnyRadix
brimScalarAnyRadix
  = either (brim radixRadCom radCom) (brim radixRadPer radPer)

hash :: Counter Hash
hash = nextChar

newline :: Counter Newline
newline Newline = ((over line succ . set column 0 . over position succ) :)

comment :: Counter Comment
comment (Comment a b c) = hash a . sequence commentChar b . newline c

space :: Counter Space
space = nextChar

-- | 'tab' makes no attempt to advance the counter multiple columns; a
-- tab is simply counted as a single column.
tab :: Counter Tab
tab = nextChar

white :: Counter White
white = caseS4 space tab newline comment

whites :: Counter Whites
whites (Whites w ws) = white w . sequence white ws

fs :: Counter a -> Counter (Fs a)
fs c (Fs a m) = c a . maybe whites m

bs :: Counter a -> Counter (Bs a)
bs c (Bs m a) = maybe whites m . c a

digitsFour :: Counter DigitsFour
digitsFour (DigitsFour a b c d)
  = d9z a . d9z b . d9z c . d9z d

digits1or2 :: Counter Digits1or2
digits1or2 (Digits1or2 a m) = d9z a . maybe d9z m

colon :: Counter Colon
colon = nextChar

hours :: Counter Hours
hours x = case x of
  H0to19 a b -> maybe d1z a . d9z b
  H20to23 a b -> two a . d3z b

time :: Counter Time
time (Time a b c d) = hours a . colon b . zeroTo59 c
  . maybe (\(a, b) -> colon a . zeroTo59 b) d

backtick :: Counter Backtick
backtick = nextChar

zone :: Counter Zone
zone (Zone a b c d e f)
  = backtick a . pluMin b . d2z c . d3z d . d9z e . d9z f

doubleQuote :: Counter DoubleQuote
doubleQuote = nextChar

backslash :: Counter Backslash
backslash = nextChar

gap :: Counter Gap
gap (Gap a b) = whites a . backslash b

escPayload :: Counter EscPayload
escPayload = caseS4 backslash newline doubleQuote gap

escSeq :: Counter EscSeq
escSeq (EscSeq a b) = backslash a . escPayload b

quotedChar :: Counter QuotedChar
quotedChar (QuotedChar ei) = either nonEscapedChar escSeq ei

quotedString :: Counter QuotedString
quotedString (QuotedString a b c) = doubleQuote a . sequence quotedChar b
  . doubleQuote c

unquotedString :: Counter UnquotedString
unquotedString (UnquotedString a b c)
  = sequence d9z a . usCharNonDigit b
  . sequence (either usCharNonDigit d9z) c

unquotedCommodity :: Counter UnquotedCommodity
unquotedCommodity (UnquotedCommodity a b)
  = usCharNonDigit a . sequence usCharNonDigit b

commodity :: Counter Commodity
commodity (Commodity ei) = either unquotedCommodity quotedString ei

nonNeutral :: Counter NonNeutral
nonNeutral x = case x of
  NonNeutralRadCom a b -> backtick a . brim radixRadCom radCom b
  NonNeutralRadPer b -> brim radixRadPer radPer b

neutralOrNon :: Counter NeutralOrNon
neutralOrNon x = case x of
  NeutralOrNonRadCom a b -> backtick a
    . either (nil radixRadCom radCom) (brim radixRadCom radCom) b
  NeutralOrNonRadPer a -> either (nil radixRadPer radPer)
    (brim radixRadPer radPer) a

neutral :: Counter Neutral
neutral x = case x of
  NeuCom a b -> backtick a . nil radixRadCom radCom b
  NeuPer a -> nil radixRadPer radPer a

lessThan :: Counter LessThan
lessThan = nextChar

greaterThan :: Counter GreaterThan
greaterThan = nextChar

dipole :: Counter Dipole
dipole = either lessThan greaterThan

trio :: Counter Trio
trio x = case x of
  QcCyOnLeft a b c -> fs dipole a . fs commodity b . nonNeutral c
  QcCyOnRight a b c -> fs dipole a . fs nonNeutral b . commodity c
  QSided a b -> fs dipole a . nonNeutral b
  QUnsided a -> neutral a
  SC a b -> fs dipole a . commodity b
  S a -> dipole a
  UcCyOnLeft a b -> fs commodity a . nonNeutral b
  UcCyOnRight a b -> fs nonNeutral a . commodity b
  U a -> nonNeutral a
  C a -> commodity a

openSquare :: Counter OpenSquare
openSquare = nextChar

closeSquare :: Counter CloseSquare
closeSquare = nextChar

integer :: Counter Integer
integer (Integer a)
  = either zero (\(a, b, c) -> maybe pluMin a . d9 b . sequence d9z c) a

scalar :: Counter Scalar
scalar = caseS6 unquotedString quotedString date time zone integer

bracketedForest :: Counter BracketedForest
bracketedForest (BracketedForest a b c)
  = fs openSquare a
  . maybe (fs forest) b
  . closeSquare c

comma :: Counter Comma
comma = nextChar

forest :: Counter Forest
forest (Forest a b)
  = tree a . sequence (\(a, b) -> bs comma a . bs tree b) b

tree :: Counter Tree
tree x = case x of
  TreeScalarFirst a b -> scalar a . maybe (bs bracketedForest) b
  TreeForestFirst a b -> bracketedForest a . maybe (bs scalar) b

openCurly :: Counter OpenCurly
openCurly = nextChar

closeCurly :: Counter CloseCurly
closeCurly = nextChar

posting :: Counter Posting
posting x = case x of
  PostingTrioFirst a b -> trio a . maybe (bs bracketedForest) b
  PostingNoTrio a -> bracketedForest a

semicolon :: Counter Semicolon
semicolon = nextChar

postingList :: Counter PostingList
postingList x = case x of
  OnePosting a -> posting a
  PostingList a b c d -> posting a . bs semicolon b . bs posting c
    . sequence (\(a, b) -> bs semicolon a . bs posting b) d

postings :: Counter Postings
postings (Postings a b c)
  = fs openCurly a . maybe (fs postingList) b . closeCurly c

transaction :: Counter Transaction
transaction (Transaction a b) = maybe (fs tree) a . postings b

atSign :: Counter AtSign
atSign = nextChar

exch :: Counter Exch
exch x = case x of
  ExchNeutral a -> neutral a
  ExchNonNeutral a b -> maybe (fs pluMin) a . nonNeutral b

cyExch :: Counter CyExch
cyExch x = case x of
  CyExchCy a b -> fs commodity a . exch b
  CyExchA a b -> fs exch a . commodity b

price :: Counter Price
price (Price a b c d e f g h)
  = fs atSign a . date b . whites c . maybe (\(a, b) -> time a . whites b) d
  . maybe (\(a, b) -> zone a . whites b) e
  . commodity f . whites g . cyExch h

fileItem :: Counter FileItem
fileItem (FileItem a) = either price transaction a

fileItems :: Counter FileItems
fileItems (FileItems a b)
  = fileItem a . sequence (\(a, b) -> whites a . fileItem b) b

ast :: Counter Ast
ast x = case x of
  AstNoLeadingWhite a -> fs fileItems a
  AstLeadingWhite a b -> whites a . maybe (fs fileItems) b
  EmptyFile -> id
