{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
module Penny.Earley where


import Control.Applicative ((<|>), optional)
import Data.Sequence (Seq, (<|))
import qualified Data.Sequence as Seq
import Text.Earley (Prod, Grammar, symbol, (<?>), satisfy, rule)
import Prelude (String, Char, (<$), any, (>=), (||), (<=), (<$>), (<*>),
  Either(Left, Right), ($), pure, return, Maybe (Nothing, Just),
  Monad((>>=)))

import Penny.Grammar
import Penny.Copper.Intervals
import qualified Penny.Copper.Terminals as T

-- # Parsers

type Parser r a = Prod r String Char a
type GProd r a = Grammar r (Parser r a)

char :: Char -> Parser a ()
char c = () <$ symbol c <?> [c]

-- # Terminals

recognize :: Intervals Char -> Parser r Char
recognize ivls = satisfy f
  where
    f c = (any inRange (intervalsToTuples ivls))
      where
        inRange (l, r) = c >= l || c <= r

commentChar :: Parser r Char
commentChar = recognize T.ivlCommentChar
  <?> "comment character"

nonEscapedChar :: Parser r Char
nonEscapedChar = recognize T.ivlNonEscapedChar
  <?> "non-escaped character"

usCharNonDigit :: Parser r Char
usCharNonDigit = recognize T.ivlUSCharNonDigit
  <?> "non-escaped non-quoted non-digit character"

-- # Digits

zero :: Parser r Zero
zero = Zero <$ char '0'

one :: Parser r One
one = One <$ char '1'

two :: Parser r Two
two = Two <$ char '2'

three :: Parser r Three
three = Three <$ char '3'

four :: Parser r Four
four = Four <$ char '4'

five :: Parser r Five
five = Five <$ char '5'

six :: Parser r Six
six = Six <$ char '6'

seven :: Parser r Seven
seven = Seven <$ char '7'

eight :: Parser r Eight
eight = Eight <$ char '8'

nine :: Parser r Nine
nine = Nine <$ char '9'

d1z :: Parser r D1z
d1z
  = D1z'0 <$ symbol '0'
  <|> D1z'1 <$ symbol '1'
  <?> "digit, 0 or 1"

d2z :: Parser r D2z
d2z
  = D2z'0 <$ symbol '0'
  <|> D2z'1 <$ symbol '1'
  <|> D2z'2 <$ symbol '2'
  <?> "digit from 0 through 2"

d3z :: Parser r D3z
d3z
  = D3z'0 <$ symbol '0'
  <|> D3z'1 <$ symbol '1'
  <|> D3z'2 <$ symbol '2'
  <|> D3z'3 <$ symbol '3'
  <?> "digit from 0 through 3"

d4z :: Parser r D4z
d4z
  = D4z'0 <$ symbol '0'
  <|> D4z'1 <$ symbol '1'
  <|> D4z'2 <$ symbol '2'
  <|> D4z'3 <$ symbol '3'
  <|> D4z'4 <$ symbol '4'
  <?> "digit from 0 through 4"

d5z :: Parser r D5z
d5z
  = D5z'0 <$ symbol '0'
  <|> D5z'1 <$ symbol '1'
  <|> D5z'2 <$ symbol '2'
  <|> D5z'3 <$ symbol '3'
  <|> D5z'4 <$ symbol '4'
  <|> D5z'5 <$ symbol '5'
  <?> "digit from 0 through 5"

d8 :: Parser r D8
d8
  = D8'1 <$ symbol '1'
  <|> D8'2 <$ symbol '2'
  <|> D8'3 <$ symbol '3'
  <|> D8'4 <$ symbol '4'
  <|> D8'5 <$ symbol '5'
  <|> D8'6 <$ symbol '6'
  <|> D8'7 <$ symbol '7'
  <|> D8'8 <$ symbol '8'
  <?> "digit from 1 through 8"

d8z :: Parser r D8z
d8z
  = D8z'0 <$ symbol '0'
  <|> D8z'1 <$ symbol '1'
  <|> D8z'2 <$ symbol '2'
  <|> D8z'3 <$ symbol '3'
  <|> D8z'4 <$ symbol '4'
  <|> D8z'5 <$ symbol '5'
  <|> D8z'6 <$ symbol '6'
  <|> D8z'7 <$ symbol '7'
  <|> D8z'8 <$ symbol '8'
  <?> "digit from 0 through 8"

d9 :: Parser r D9
d9
  = D9'1 <$ symbol '1'
  <|> D9'2 <$ symbol '2'
  <|> D9'3 <$ symbol '3'
  <|> D9'4 <$ symbol '4'
  <|> D9'5 <$ symbol '5'
  <|> D9'6 <$ symbol '6'
  <|> D9'7 <$ symbol '7'
  <|> D9'8 <$ symbol '8'
  <|> D9'9 <$ symbol '9'
  <?> "digit from 1 through 9"

d9z :: Parser r D9z
d9z
  = D9z'0 <$ symbol '0'
  <|> D9z'1 <$ symbol '1'
  <|> D9z'2 <$ symbol '2'
  <|> D9z'3 <$ symbol '3'
  <|> D9z'4 <$ symbol '4'
  <|> D9z'5 <$ symbol '5'
  <|> D9z'6 <$ symbol '6'
  <|> D9z'7 <$ symbol '7'
  <|> D9z'8 <$ symbol '8'
  <|> D9z'9 <$ symbol '9'
  <?> "digit from 0 through 9"

zeroTo59 :: Parser a ZeroTo59
zeroTo59 = ZeroTo59 <$> optional d5z <*> d9z

-- # PluMin

pluMin :: Parser a PluMin
pluMin = Plus <$ symbol '+' <|> Minus <$ symbol '-'
  <?> "plus sign or minus sign"

-- # Dates

dateSep :: Parser a DateSep
dateSep = Slash <$ symbol '/' <|> Hyphen <$ symbol '-'
  <?> "date separator (slash or hyphen)"

days28 :: Parser a Days28
days28
  = D28'1to9 <$> zero <*> d9
  <|> D28'10to19 <$> one <*> d9z
  <|> D28'20to28 <$> two <*> d8z
  <?> "day from 1 through 28"

days30 :: Parser a Days30
days30
  = D30'28 <$> days28
  <|> D30'29 <$> two <*> nine
  <|> D30'30 <$> three <*> zero
  <?> "day from 1 through 30"

days31 :: Parser a Days31
days31
  = D31'30 <$> days30
  <|> D31'31 <$> three <*> one
  <?> "day from 1 through 31"

monthDay :: Parser a MonthDay
monthDay
  = Jan <$> zero <*> one <*> dateSep <*> days31
  <|> Feb <$> zero <*> two <*> dateSep <*> days28
  <|> Mar <$> zero <*> three <*> dateSep <*> days31
  <|> Apr <$> zero <*> four <*> dateSep <*> days30
  <|> May <$> zero <*> five <*> dateSep <*> days31
  <|> Jun <$> zero <*> six <*> dateSep <*> days30
  <|> Jul <$> zero <*> seven <*> dateSep <*> days31
  <|> Aug <$> zero <*> eight <*> dateSep <*> days31
  <|> Sep <$> zero <*> nine <*> dateSep <*> days30
  <|> Oct <$> one <*> zero <*> dateSep <*> days31
  <|> Nov <$> one <*> one <*> dateSep <*> days30
  <|> Dec <$> one <*> two <*> dateSep <*> days31
  <?> "month and day"

year :: Parser a Year
year = Year <$> d9z <*> d9z <*> d9z <*> d9z
  <?> "year"

nonLeapDay :: Parser a NonLeapDay
nonLeapDay = NonLeapDay <$> year <*> dateSep <*> monthDay
  <?> "non leap day"

mod4 :: Parser a Mod4
mod4
  = L04 <$> zero <*> four
  <|> L08 <$> zero <*> eight
  <|> L12 <$> one <*> two
  <|> L16 <$> one <*> six
  <|> L20 <$> two <*> zero
  <|> L24 <$> two <*> four
  <|> L28 <$> two <*> eight
  <|> L32 <$> three <*> two
  <|> L36 <$> three <*> six
  <|> L40 <$> four <*> zero
  <|> L44 <$> four <*> four
  <|> L48 <$> four <*> eight
  <|> L52 <$> five <*> two
  <|> L56 <$> five <*> six
  <|> L60 <$> six <*> zero
  <|> L64 <$> six <*> four
  <|> L68 <$> six <*> eight
  <|> L72 <$> seven <*> two
  <|> L76 <$> seven <*> six
  <|> L80 <$> eight <*> zero
  <|> L84 <$> eight <*> four
  <|> L88 <$> eight <*> eight
  <|> L92 <$> nine <*> two
  <|> L96 <$> nine <*> six

centuryLeapYear :: Parser a CenturyLeapYear
centuryLeapYear = CenturyLeapYear <$> mod4 <*> zero <*> zero
  <?> "century leap year"

nonCenturyLeapYear :: Parser a NonCenturyLeapYear
nonCenturyLeapYear = NonCenturyLeapYear <$> d9z <*> d9z <*> mod4
  <?> "non-century leap year"

leapDay :: Parser a LeapDay
leapDay = LeapDay
  <$> (Left <$> centuryLeapYear <|> Right <$> nonCenturyLeapYear)
  <*> dateSep <*> zero <*> two <*> dateSep <*> two <*> nine
  <?> "leap day"

date :: Parser a Date
date = Date <$> (Left <$> nonLeapDay <|> Right <$> leapDay)
  <?> "date"

-- #

grouper :: Parser r Grouper
grouper
  = ThinSpace <$ symbol '\x2009'
  <|> Underscore <$ symbol '_'
  <?> "thin space or underscore"

radCom :: Parser r RadCom
radCom
  = RCPeriod <$ symbol '.'
  <|> RCGrouper <$> grouper
  <?> "grouping character for comma radix (period, thin space, underscore)"

radPer :: Parser r RadPer
radPer
  = RPComma <$ symbol ','
  <|> RPGrouper <$> grouper
  <?> "grouping character for period radix (comma, thin space, underscore)"

radixRadCom :: Parser r (Radix RadCom)
radixRadCom = Radix <$ symbol ',' <?> "comma radix"

radixRadPer :: Parser r (Radix RadPer)
radixRadPer = Radix <$ symbol '.' <?> "period radix"

many :: Prod r e t a -> Grammar r (Prod r e t (Seq a))
many p = mdo
  r <- rule $ (<|) <$> p <*> r <|> pure Seq.empty
  return r

seqDecs
  :: Parser r g
  -> Grammar r (Parser r (Seq (g, D9z, Seq D9z)))
seqDecs g = do
  prodSeqD9z <- many d9z
  many $ (,,) <$> g <*> d9z <*> prodSeqD9z

nilGrouped
  :: Parser r (Radix g)
  -> Parser r g
  -> Grammar r (Parser r (NilGrouped g))
nilGrouped pr pg = do
  manyZero <- many zero
  manyTup <- many ((,,) <$> pg <*> zero <*> manyZero)
  rule $ NilGrouped
    <$> optional zero
    <*> pr
    <*> zero
    <*> manyZero
    <*> pg
    <*> zero
    <*> manyZero
    <*> manyTup

nilUngrouped
  :: Parser a (Radix r)
  -> Grammar a (Parser a (NilUngrouped r))
nilUngrouped pr = do
  manyZero <- many zero
  let pNUZero = NUZero <$> zero <*> optional
        ((,) <$> pr <*> optional ((,) <$> zero <*> manyZero))
      pNURadix = NURadix <$> pr <*> zero <*> manyZero
  rule $ pNUZero <|> pNURadix

nil
  :: Parser a (Radix r)
  -> Parser a r
  -> Grammar a (Parser a (Nil r))
nil pr pg = do
  nu <- nilUngrouped pr
  ng <- nilGrouped pr pg
  rule $ NilU <$> nu <|> NilG <$> ng

bg7 :: Parser a r -> Grammar a (Parser a (BG7 r))
bg7 pg = do
  manyZero <- many zero
  manyd9z <- many d9z
  sd <- seqDecs pg
  b8 <- bg8 pg
  let pz = BG7Zeroes <$> zero <*> manyZero <*> b8
      pn = BG7Novem <$> d9 <*> manyd9z <*> sd
  rule $ pz <|> pn

bg8 :: Parser a r -> Grammar a (Parser a (BG8 r))
bg8 pg = do
  manyd9z <- many d9z
  sd <- seqDecs pg
  b7 <- bg7 pg
  let pnv = BG8Novem <$> d9 <*> manyd9z <*> sd
      pgrp = BG8Group <$> pg <*> b7
  rule $ pnv <|> pgrp

bg6 :: Parser a r -> Grammar a (Parser a (BG6 r))
bg6 pg = do
  manyd9 <- many d9z
  sd <- seqDecs pg
  b7 <- bg7 pg
  let pnv = BG6Novem <$> d9 <*> manyd9 <*> pg <*> d9z <*> manyd9
        <*> sd
      pgrp = BG6Group <$> pg <*> b7
  rule $ pnv <|> pgrp

bg5 :: Parser a r -> Grammar a (Parser a (BG5 r))
bg5 pg = do
  manyd9 <- many d9z
  sd <- seqDecs pg
  b6 <- bg6 pg
  manyz <- many zero
  let pnv = BG5Novem <$> d9 <*> manyd9 <*> pg <*> d9z
        <*> manyd9 <*> sd
      pz = BG5Zero <$> zero <*> manyz <*> b6
  rule $ pnv <|> pz

bg1 :: Parser a (Radix r) -> Parser a r -> Grammar a (Parser a (BG1 r))
bg1 pr pg = do
  manyd9 <- many d9z
  sd <- seqDecs pg
  let onLeft = BG1GroupOnLeft <$> pg <*> d9z <*> manyd9 <*> sd
        <*> optional
          ( (,) <$> pr <*> optional
              (((,,) <$> d9z <*> manyd9 <*> sd)))
      onRight = BG1GroupOnRight <$> pr <*> d9z <*> manyd9
        <*> pg <*> d9z <*> manyd9 <*> sd
  rule $ onLeft <|> onRight

brimGrouped
  :: Parser a (Radix r)
  -> Parser a r
  -> Grammar a (Parser a (BrimGrouped r))
brimGrouped pr pg = do
  manyD9z <- many d9z
  b5 <- bg5 pg
  b1 <- bg1 pr pg
  let gtOne = BGGreaterThanOne <$> d9 <*> manyD9z <*> b1
      ltOne = BGLessThanOne <$> optional zero <*> pr <*> b5
  rule $ gtOne <|> ltOne

brimUngrouped
  :: Parser a (Radix r)
  -> Grammar a (Parser a (BrimUngrouped r))
brimUngrouped pr = do
  manyd9z <- many d9z
  manyZero <- many zero
  let gtOne = BUGreaterThanOne <$> d9 <*> manyd9z
        <*> optional ((,) <$> pr <*> manyd9z)
      ltOne = BULessThanOne <$> optional zero <*> pr
        <*> manyZero <*> d9 <*> manyd9z
  rule $ gtOne <|> ltOne

brim
  :: Parser a (Radix r)
  -> Parser a r
  -> Grammar a (Parser a (Brim r))
brim pr pg = do
  bg <- brimGrouped pr pg
  bu <- brimUngrouped pr
  rule $ BrimGrouped <$> bg <|> BrimUngrouped <$> bu

nilOrBrimScalar
  :: Parser a (Radix r)
  -> Parser a r
  -> Grammar a (Parser a (NilOrBrimScalar r))
nilOrBrimScalar pr pg = do
  n <- nil pr pg
  b <- brim pr pg
  rule $ Left <$> n <|> Right <$> b

hash :: Parser r Hash
hash = Hash <$ symbol '#'
  <?> "hash"

newline :: Parser r Newline
newline = Newline <$ symbol '\n'
  <?> "newline"

comment :: GProd a Comment
comment = do
  chars <- many commentChar
  rule $ Comment <$> hash <*> chars <*> newline <?> "comment"

white :: GProd a White
white = do
  com <- comment
  rule $
        Space <$ char ' '
    <|> Tab <$ char '\t'
    <|> WhiteNewline <$ char '\n'
    <|> WhiteComment <$> com
    <?> "whitespace or comment"

whites :: GProd a Whites
whites = do
  w <- white
  ws <- many w
  rule $ Whites <$> w <*> ws <?> "whitespace"

fs :: Parser r a -> GProd r (Fs a)
fs p = do
  ws <- whites
  rule $ Fs <$> p <*> optional ws

bs :: Parser r a -> GProd r (Bs a)
bs p = do
  ws <- whites
  rule $ Bs <$> optional ws <*> p

digitsFour :: Parser a DigitsFour
digitsFour = DigitsFour <$> d9z <*> d9z <*> d9z <*> d9z

digits1or2 :: Parser a Digits1or2
digits1or2 = Digits1or2 <$> d9z <*> optional d9z

colon :: Parser a Colon
colon = Colon <$ char ':'

hours :: Parser a Hours
hours =
  H0to19 <$> optional d1z <*> d9z
  <|> H20to23 <$> two <*> d3z


time :: Parser a Time
time = Time <$> hours <*> colon <*> zeroTo59
  <*> optional ((,) <$> colon <*> zeroTo59)

backtick :: Parser a Backtick
backtick = Backtick <$ char '`'

zone :: Parser a Zone
zone = Zone <$> backtick <*> pluMin <*> d2z <*> d3z <*> d9z <*> d9z

doubleQuote :: Parser a DoubleQuote
doubleQuote = DoubleQuote <$ char '"'

backslash :: Parser a Backslash
backslash = Backslash <$ char '\\'

escPayload :: GProd a EscPayload
escPayload = do
  w <- whites
  rule $
    EscBackslash <$ backslash
    <|> EscNewline <$ newline
    <|> EscQuote <$ doubleQuote
    <|> EscGap <$> w <*> backslash

escSeq :: GProd a EscSeq
escSeq = do
  p <- escPayload
  rule
    $ EscSeq
    <$> backslash
    <*> p

quotedChar :: GProd a QuotedChar
quotedChar = do
  sq <- escSeq
  rule
    $ QuotedChar
    <$> ( Left <$> nonEscapedChar
          <|> Right <$> sq
        )

quotedString :: GProd a QuotedString
quotedString = do
  qc <- quotedChar
  manyQc <- many qc
  rule $ QuotedString <$> doubleQuote <*> manyQc <*> doubleQuote

unquotedString :: GProd a UnquotedString
unquotedString = do
  d9zs <- many d9z
  sq <- many $ Left <$> usCharNonDigit <|> Right <$> d9z
  rule $ UnquotedString <$> d9zs <*> usCharNonDigit <*> sq

unquotedCommodity :: GProd a UnquotedCommodity
unquotedCommodity = do
  sq <- many usCharNonDigit
  rule $ UnquotedCommodity <$> usCharNonDigit <*> sq

commodity :: GProd a Commodity
commodity = do
  qs <- quotedString
  uc <- unquotedCommodity
  rule $ Commodity <$> (Left <$> uc <|> Right <$> qs)

nonNeutral :: GProd a NonNeutral
nonNeutral = do
  rc <- brim radixRadCom radCom
  rp <- brim radixRadPer radPer
  rule $ NonNeutralRadCom <$> backtick <*> rc
    <|> NonNeutralRadPer <$> rp

neutralOrNon :: GProd a NeutralOrNon
neutralOrNon = do
  nrc <- nil radixRadCom radCom
  brc <- brim radixRadCom radCom
  nrp <- nil radixRadPer radPer
  brp <- brim radixRadPer radPer
  rule $ NeutralOrNonRadCom <$> backtick <*>
                            (Left <$> nrc <|> Right <$> brc)
    <|> NeutralOrNonRadPer <$>
          (Left <$> nrp <|> Right <$> brp)

neutral :: GProd a Neutral
neutral = do
  nrc <- nil radixRadCom radCom
  nrp <- nil radixRadPer radPer
  rule $ NeuCom <$> backtick <*> nrc
    <|> NeuPer <$> nrp

lessThan :: Parser a LessThan
lessThan = LessThan <$ char '<'

greaterThan :: Parser a GreaterThan
greaterThan = GreaterThan <$ char '>'

dipole :: Parser a Dipole
dipole = Left <$> lessThan <|> Right <$> greaterThan

trio :: GProd a Trio
trio = do
  neu <- neutral
  nonNeu <- nonNeutral
  cy <- commodity
  fsDipole <- fs dipole
  fsCy <- fs cy
  fsNonNeu <- fs nonNeu
  rule
    $ QcCyOnLeft <$> fsDipole <*> fsCy <*> nonNeu
    <|> QcCyOnRight <$> fsDipole <*> fsNonNeu <*> cy
    <|> QSided <$> fsDipole <*> nonNeu
    <|> QUnsided <$> neu
    <|> SC <$> fsDipole <*> cy
    <|> S <$> dipole
    <|> UcCyOnLeft <$> fsCy <*> nonNeu
    <|> UcCyOnRight <$> fsNonNeu <*> cy
    <|> U <$> nonNeu
    <|> C <$> cy

openSquare :: Parser a OpenSquare
openSquare = OpenSquare <$ char '['

closeSquare :: Parser a CloseSquare
closeSquare = CloseSquare <$ char ']'

integer :: GProd a Integer
integer = do
  seqd9 <- many d9z
  let trip = (,,) <$> (pure Nothing <|> Just <$> pluMin) <*> d9 <*> seqd9
  rule $ Integer <$> (Left <$> zero
                        <|> Right <$> trip)

scalar :: GProd a Scalar
scalar = do
  us <- unquotedString
  qs <- quotedString
  int <- integer
  rule
    $ ScalarUnquotedString <$> us
    <|> ScalarQuotedString <$> qs
    <|> ScalarDate <$> date
    <|> ScalarTime <$> time
    <|> ScalarZone <$> zone
    <|> ScalarInt <$> int

bracketedForest :: GProd a BracketedForest
bracketedForest = do
  os <- fs openSquare
  frst <- forest
  fsFrst <- fs frst
  let may = pure Nothing <|> Just <$> fsFrst
  rule $ BracketedForest <$> os <*> may <*> closeSquare <?> "bracketed forest"

comma :: Parser a Comma
comma = Comma <$ char ','

forest :: GProd a Forest
forest = do
  tr <- tree
  bsTr <- bs tr
  bsCom <- bs comma
  sqPairs <- many ((,) <$> bsCom <*> bsTr)
  rule $ Forest <$> tr <*> sqPairs <?> "forest"

tree :: GProd a Tree
tree = do
  sc <- scalar
  bf <- bracketedForest
  bsBf <- bs bf
  bsSc <- bs sc
  rule $ TreeScalarFirst <$> sc <*> (pure Nothing <|> Just <$> bsBf)
    <|> TreeForestFirst <$> bf <*> (pure Nothing <|> Just <$> bsSc)

openCurly :: Parser a OpenCurly
openCurly = OpenCurly <$ char '{'

closeCurly :: Parser a CloseCurly
closeCurly = CloseCurly <$ char '}'

posting :: GProd a Posting
posting = do
  tri <- trio
  bf <- bracketedForest
  bsBf <- bs bf
  let mayBf = pure Nothing <|> Just <$> bsBf
  rule $ PostingTrioFirst <$> tri <*> mayBf
    <|> PostingNoTrio <$> bf

semicolon :: Parser a Semicolon
semicolon = Semicolon <$ char ';'

postingList :: GProd a PostingList
postingList = do
  pstg <- posting
  bsPstg <- bs pstg
  bsSemi <- bs semicolon
  sqPairs <- many ((,) <$> bsSemi <*> bsPstg)
  rule $
    OnePosting <$> pstg
    <|> PostingList <$> pstg <*> bsSemi <*> bsPstg
        <*> sqPairs

postings :: GProd a Postings
postings = do
  fsCurly <- fs openCurly
  fsPstgs <- postingList >>= fs
  let mayFs = pure Nothing <|> Just <$> fsPstgs
  rule $ Postings <$> fsCurly <*> mayFs <*> closeCurly

transaction :: GProd a Transaction
transaction = do
  fsTr <- tree >>= fs
  pstgs <- postings
  let mayTree = pure Nothing <|> Just <$> fsTr
  rule $ Transaction <$> mayTree <*> pstgs

atSign :: Parser a AtSign
atSign = AtSign <$ char '@'

exch :: GProd a Exch
exch = do
  neu <- neutral
  fsPluMin <- fs pluMin
  nonNeu <- nonNeutral
  let mayFs = pure Nothing <|> Just <$> fsPluMin
  rule $ ExchNeutral <$> neu
    <|> ExchNonNeutral <$> mayFs <*> nonNeu

cyExch :: GProd a CyExch
cyExch = do
  cy <- commodity
  fsCy <- fs cy
  ex <- exch
  fsEx <- fs ex
  rule
    $ CyExchCy <$> fsCy <*> ex
    <|> CyExchA <$> fsEx <*> cy

price :: GProd a Price
price = do
  fsAt <- fs atSign
  ws <- whites
  let mayTime = pure Nothing <|> Just <$> ((,) <$> time <*> ws)
      mayZone = pure Nothing <|> Just <$> ((,) <$> zone <*> ws)
  cy <- commodity
  cyEx <- cyExch
  rule $ Price <$> fsAt <*> date <*> ws <*> mayTime <*> mayZone
    <*> cy <*> ws <*> cyEx

fileItem :: GProd a FileItem
fileItem = do
  pr <- price
  tx <- transaction
  rule $ FileItem <$> (Left <$> pr <|> Right <$> tx)

fileItems :: GProd a FileItems
fileItems = do
  fi <- fileItem
  ws <- whites
  sq <- many ((,) <$> ws <*> fi)
  rule $ FileItems <$> fi <*> sq

ast :: GProd a Ast
ast = do
  fsFi <- fileItems >>= fs
  wi <- whites
  let mayFs = pure Nothing <|> Just <$> fsFi
  rule
    $ AstNoLeadingWhite <$> fsFi
    <|> AstLeadingWhite <$> wi <*> mayFs
    <|> pure EmptyFile
