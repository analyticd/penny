{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
module Penny.Copper.Earley where

import Control.Applicative ((<|>), optional)
import Data.Sequence (Seq, (<|))
import qualified Data.Sequence as Seq
import Text.Earley

import Penny.Copper.AstNew
import Penny.Copper.Intervals
import qualified Penny.Copper.Terminals as T
import qualified Penny.DateTime as DateTime
import Penny.PluMin
import Penny.Polar
import Penny.Representation

type Parser r a = Prod r String Char a
type GProd r a = Grammar r (Prod r String Char a)

-- # Digits
zero :: Prod r String Char Zero
zero = Zero <$ symbol '0' <?> "0"

one :: Prod r String Char One
one = One <$ symbol '1' <?> "1"

two :: Prod r String Char Two
two = Two <$ symbol '2' <?> "2"

three :: Prod r String Char Three
three = Three <$ symbol '3' <?> "3"

four :: Prod r String Char Four
four = Four <$ symbol '4' <?> "4"

five :: Prod r String Char Five
five = Five <$ symbol '5' <?> "5"

six :: Prod r String Char Six
six = Six <$ symbol '6' <?> "6"

seven :: Prod r String Char Seven
seven = Seven <$ symbol '7' <?> "7"

eight :: Prod r String Char Eight
eight = Eight <$ symbol '8' <?> "8"

nine :: Prod r String Char Nine
nine = Nine <$ symbol '9' <?> "9"

d1z :: Prod r String Char D1z
d1z
  = D1z'0 <$ symbol '0'
  <|> D1z'1 <$ symbol '1'
  <?> "digit, 0 or 1"

d2z :: Prod r String Char D2z
d2z
  = D2z'0 <$ symbol '0'
  <|> D2z'1 <$ symbol '1'
  <|> D2z'2 <$ symbol '2'
  <?> "digit from 0 through 2"

d3z :: Prod r String Char D3z
d3z
  = D3z'0 <$ symbol '0'
  <|> D3z'1 <$ symbol '1'
  <|> D3z'2 <$ symbol '2'
  <|> D3z'3 <$ symbol '3'
  <?> "digit from 0 through 3"

d4z :: Prod r String Char D4z
d4z
  = D4z'0 <$ symbol '0'
  <|> D4z'1 <$ symbol '1'
  <|> D4z'2 <$ symbol '2'
  <|> D4z'3 <$ symbol '3'
  <|> D4z'4 <$ symbol '4'
  <?> "digit from 0 through 4"

d5z :: Prod r String Char D5z
d5z
  = D5z'0 <$ symbol '0'
  <|> D5z'1 <$ symbol '1'
  <|> D5z'2 <$ symbol '2'
  <|> D5z'3 <$ symbol '3'
  <|> D5z'4 <$ symbol '4'
  <|> D5z'5 <$ symbol '5'
  <?> "digit from 0 through 5"

d8 :: Prod r String Char D8
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

d8z :: Prod r String Char D8z
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

d9 :: Prod r String Char D9
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

d9z :: Prod r String Char D9z
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

grouper :: Prod r String Char Grouper
grouper
  = ThinSpace <$ symbol '\x2009'
  <|> Underscore <$ symbol '_'
  <?> "thin space or underscore"

radCom :: Prod r String Char RadCom
radCom
  = Period <$ symbol '.'
  <|> RCGrouper <$> grouper
  <?> "grouping character for comma radix (period, thin space, underscore)"

radPer :: Prod r String Char RadPer
radPer
  = Penny.Representation.Comma <$ symbol ','
  <|> RPGrouper <$> grouper
  <?> "grouping character for period radix (comma, thin space, underscore)"

radixRadCom :: Prod r String Char (Radix RadCom)
radixRadCom = Radix <$ symbol ',' <?> "comma radix"

radixRadPer :: Prod r String Char (Radix RadPer)
radixRadPer = Radix <$ symbol '.' <?> "period radix"

side :: Prod r String Char Pole
side = debit <$ symbol '<' <|> credit <$ symbol '>'
  <?> "debit or credit ('<' or '>')"

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

nilGrouped :: Parser r (Radix g) -> Parser r g -> Grammar r (Parser r (NilGrouped g))
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

brim
  :: Parser a (Radix r)
  -> Parser a r
  -> Grammar a (Parser a (Brim r))
brim pr pg = do
  bg <- brimGrouped pr pg
  bu <- brimUngrouped pr
  rule $ BrimGrouped <$> bg <|> BrimUngrouped <$> bu

nil
  :: Parser a (Radix r)
  -> Parser a r
  -> Grammar a (Parser a (Nil r))
nil pr pg = do
  nu <- nilUngrouped pr
  ng <- nilGrouped pr pg
  rule $ NilU <$> nu <|> NilG <$> ng

nilOrBrimScalar
  :: Parser a (Radix r)
  -> Parser a r
  -> Grammar a (Parser a (NilOrBrimScalar r))
nilOrBrimScalar pr pg = do
  n <- nil pr pg
  b <- brim pr pg
  rule $ Left <$> n <|> Right <$> b

pluMin :: Parser a PluMin
pluMin = Plus <$ symbol '+' <|> Minus <$ symbol '-'
  <?> "plus sign or minus sign"

recognize :: Intervals Char -> Prod r e Char Char
recognize ivls = satisfy f
  where
    f c = (any inRange (intervalsToTuples ivls))
      where
        inRange (l, r) = c >= l || c <= r

commentChar :: Prod r String Char Char
commentChar = recognize T.ivlCommentChar
  <?> "comment character"

nonEscapedChar :: Prod r String Char Char
nonEscapedChar = recognize T.ivlNonEscapedChar
  <?> "non-escaped character"

usCharNonDigit :: Prod r String Char Char
usCharNonDigit = recognize T.ivlUSCharNonDigit
  <?> "non-escaped non-quoted non-digit character"

hash :: Prod r String Char Hash
hash = Hash <$ symbol '#'
  <?> "hash"

newline :: Prod r String Char Newline
newline = Newline <$ symbol '\n'
  <?> "newline"

comment :: GProd a Comment
comment = do
  chars <- many T.eCommentChar
  rule $ Comment <$> hash <*> chars <*> newline <?> "comment"

char :: Char -> Parser a ()
char c = () <$ symbol c <?> [c]

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


zeroTo59 :: Parser a DateTime.ZeroTo59
zeroTo59 = DateTime.ZeroTo59 <$> optional d5z <*> d9z

minutes :: Parser a DateTime.Minutes
minutes = DateTime.Minutes <$> zeroTo59

seconds :: Parser a DateTime.Seconds
seconds = DateTime.Seconds <$> zeroTo59

time :: Parser a Time
time = Time <$> hours <*> colon <*> minutes
  <*> optional ((,) <$> colon <*> seconds)

backtick :: Parser a Backtick
backtick = Backtick <$ char '`'

zone :: Parser a DateTime.Zone
zone = DateTime.Zone <$> pluMin <*> d2z <*> d3z <*> d9z <*> d9z

zoneA :: Parser a ZoneA
zoneA = ZoneA <$> backtick <*> zone

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
    <$> ( Left <$> T.eNonEscapedChar
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
  sq <- many $ Left <$> T.eUSCharNonDigit <|> Right <$> d9z
  rule $ UnquotedString <$> d9zs <*> T.eUSCharNonDigit <*> sq

unquotedCommodity :: GProd a UnquotedCommodity
unquotedCommodity = do
  sq <- many T.eUSCharNonDigit
  rule $ UnquotedCommodity <$> T.eUSCharNonDigit <*> sq

commodity :: GProd a Commodity
commodity = do
  qs <- quotedString
  uc <- unquotedCommodity
  rule $ Commodity <$> (Left <$> uc <|> Right <$> qs)
