module Penny.Denver.Parser (ledger) where

import Control.Applicative(
  (<$>), (<*>), (<*), (*>), (<$), many, pure,
  (<|>), optional)
import Control.Monad (replicateM)
import qualified Data.Char as Char
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Text as X
import qualified Data.Time as T
import qualified Penny.Lincoln as L
import Text.Parsec (
  char, digit, satisfy, option, optionMaybe,
  letter, alphaNum, try, string, notFollowedBy,
  sepBy1, eof)
import qualified Penny.Copper as Cop
import qualified Penny.Copper.Qty as Q
import qualified Penny.Denver.Posting as P
import Penny.Denver.Transaction (lincolnize, Raw(Raw))
import Text.Parsec.Text (Parser)

import qualified Penny.Denver.Common as C
import qualified Penny.Denver.TopLine as T

-- | Converts a parser to one that parses itself and then any trailing
-- whitespace.
lexeme :: Parser a -> Parser a
lexeme p = p <* many (char ' ')

cleared :: Parser C.Cleared
cleared = char '*' *> pure C.Cleared
          <|> pure C.NotCleared

day :: Parser T.Day
day = do
  y <- read <$> replicateM 4 digit
  _ <- char '-' <|> char '/'
  m <- read <$> replicateM 2 digit
  _ <- char '-' <|> char '/'
  d <- read <$> replicateM 2 digit
  case T.fromGregorianValid y m d of
    Nothing -> fail "invalid day"
    Just dy -> return dy

number :: Parser L.Number
number =
  L.Number
  <$ char '('
  <*> (L.TextNonEmpty
       <$> satisfy (/= ')')
       <*> (X.pack <$> (many (satisfy (/= ')')))))
  <*  char ')'

isPayeeChar :: Char -> Bool
isPayeeChar c = not $ c `elem` "\t\n"

payee :: Parser L.Payee
payee =
  L.Payee
  <$> (L.TextNonEmpty
       <$> satisfy isPayeeChar
       <*> (X.pack <$> many (satisfy isPayeeChar)))

topLine :: Parser T.TopLine
topLine =
  T.TopLine
  <$> lexeme day
  <*> lexeme (option C.NotCleared cleared)
  <*> lexeme (optionMaybe number)
  <*> optionMaybe payee
  <* eol

negative :: Parser P.Sign
negative = char '-' *> pure P.Negative

isCurrencySymbol :: Char -> Bool
isCurrencySymbol c = Char.generalCategory c == Char.CurrencySymbol

qty :: Parser L.Qty
qty = Q.qtyUnquoted Q.periodComma

commodity :: Parser P.Commodity
commodity = currency <|> named where
  currency = P.Commodity
             <$> (L.TextNonEmpty
                  <$> satisfy isCurrencySymbol
                  <*> pure X.empty)
  named = P.Commodity
          <$> (L.TextNonEmpty
               <$> letter
               <*> (X.pack <$> many alphaNum))

{-
Possible combinations for entries. Things in brackets are optional.
C means Commodity, Q means Qty, N means Negative sign, P means Space.
Spaces here are for readability only and do not have meaning.

Commodity on left side:
[N] C [P] [N] Q

Commodity on right side:
[N] Q [P] C

-}

commodityOnLeft ::
  Bool -- ^ Negative sign?
  -> P.Commodity
  -> Bool -- ^ Space?
  -> Bool -- ^ Negative sign?
  -> L.Qty
  -> Maybe P.Entry
commodityOnLeft n1 c s n2 q = let
  sign = case (n1, n2) of
    (True, True) -> Nothing
    (True, False) -> Just P.Negative
    (False, True) -> Just P.Negative
    (False, False) -> Just P.Positive
  fmt = L.Format L.CommodityOnLeft btwn
  btwn = if s then L.SpaceBetween else L.NoSpaceBetween
  amt = P.Amount q c
  in case sign of
    Nothing -> Nothing
    Just sgn -> Just $ P.Entry sgn amt fmt

commodityOnRight ::
  Bool -- ^ Negative?
  -> L.Qty
  -> Bool -- ^ Space?
  -> P.Commodity
  -> P.Entry
commodityOnRight n q s c = P.Entry sgn amt fmt where
  sgn = if n then P.Negative else P.Positive
  amt = P.Amount q c
  fmt = L.Format L.CommodityOnRight bet
  bet = if s then L.SpaceBetween else L.NoSpaceBetween

parseCtyOnLeft :: Parser P.Entry
parseCtyOnLeft = let
  p = commodityOnLeft
      <$> option False (negative *> pure True)
      <*> commodity
      <*> option False (char ' ' *> pure True)
      <*> option False (negative *> pure True)
      <*> qty
  in do
    maybeEn <- p
    case maybeEn of
      Nothing -> fail "two negative signs"
      Just e -> return e

parseCtyOnRight :: Parser P.Entry
parseCtyOnRight = commodityOnRight
                  <$> option False (negative *> pure True)
                  <*> qty
                  <*> option False (char ' ' *> pure True)
                  <*> commodity

entry :: Parser P.Entry
entry = try (parseCtyOnLeft) <|> parseCtyOnRight

{-
Possible combinations for prices. Things in brackets are optional.
C means Commodity, Q means Qty, P means Space.

Commodity on left side:
@ P C [P] Q

Commodity on right side:
@ P Q [P] C
-}

priceCtyOnLeft ::
  P.Commodity
  -> Bool -- ^ Space?
  -> L.Qty
  -> P.Price
priceCtyOnLeft cty sp q = P.Price cty q fmt where
  fmt = L.Format L.CommodityOnLeft s
  s = if sp then L.SpaceBetween else L.NoSpaceBetween

priceCtyOnRight ::
  L.Qty
  -> Bool -- ^ Space?
  -> P.Commodity
  -> P.Price
priceCtyOnRight q sp cty = P.Price cty q fmt where
  fmt = L.Format L.CommodityOnRight s
  s = if sp then L.SpaceBetween else L.NoSpaceBetween

parsePriceCtyOnLeft :: Parser P.Price
parsePriceCtyOnLeft =
  priceCtyOnLeft
  <$> commodity
  <*> option False (char ' ' *> pure True)
  <*> qty

parsePriceCtyOnRight :: Parser P.Price
parsePriceCtyOnRight =
  priceCtyOnRight
  <$> qty
  <*> option False (char ' ' *> pure True)
  <*> commodity

parsePrice :: Parser P.Price
parsePrice =
  string "@ "
  *> (parsePriceCtyOnLeft <|> parsePriceCtyOnRight)

record :: Parser P.Record
record = P.Record
         <$> lexeme entry
         <*> optional parsePrice

isMemoChar :: Char -> Bool
isMemoChar c = not $ c `elem` "\t\n"

memoLine :: Parser L.MemoLine
memoLine =
  L.MemoLine
  <$ char ';'
  <*> (L.TextNonEmpty
       <$> satisfy isMemoChar
       <*> (X.pack <$> many (satisfy isMemoChar)))

eol :: Parser ()
eol = char '\n' *> many (char ' ') *> pure ()

isFirstAccountChar :: Char -> Bool
isFirstAccountChar = Char.isAlpha

isOtherAccountChar :: Char -> Bool
isOtherAccountChar = Char.isAlphaNum

accountSpace :: Parser Char
accountSpace = try (char ' ' <* notFollowedBy (char ' '))

otherAccountChar :: Parser Char
otherAccountChar = satisfy isOtherAccountChar
                   <|> accountSpace

firstSubAccount :: Parser L.SubAccountName
firstSubAccount =
  L.SubAccountName
  <$> (L.TextNonEmpty
       <$> satisfy isFirstAccountChar
       <*> (X.pack <$> many otherAccountChar))

otherSubAccount :: Parser L.SubAccountName
otherSubAccount =
  L.SubAccountName
  <$> (L.TextNonEmpty
       <$> satisfy isOtherAccountChar
       <*> (X.pack <$> many otherAccountChar))

otherSubAccounts :: Parser [L.SubAccountName]
otherSubAccounts = char ':' *> sepBy1 otherSubAccount (char ':')

account :: Parser L.Account
account = L.Account
          <$> ((:|)
               <$> firstSubAccount
               <*> option [] otherSubAccounts)

posting :: Parser P.Posting
posting =
  P.Posting
  <$> lexeme cleared
  <*> lexeme account
  <*> lexeme (optional record)
  <*> lexeme (optional memoLine)
  <* eol

family :: Parser Raw
family =
  Raw <$> (L.Family
           <$> topLine
           <*> posting
           <*> posting
           <*> many posting)

transaction :: Parser (L.TransactionBox, [L.PriceBox])
transaction = do
  raw <- family
  case lincolnize raw of
    Nothing -> fail "lincolnization failed"
    Just pair -> return pair

isCommentChar :: Char -> Bool
isCommentChar c = not $ c `elem` "\t\n"

comment :: Parser Cop.Item
comment =
  Cop.CommentItem
  <$> (Cop.Comment
       <$ char ';'
       <*> (X.pack <$> many (satisfy isCommentChar))
       <* eol)

blankLine :: Parser Cop.Item
blankLine = many (char ' ') *> eol *> pure Cop.BlankLine

transactionPairToItem ::
  (L.TransactionBox, [L.PriceBox]) -> NonEmpty Cop.Item
transactionPairToItem (t, ps) = let
  tItem = Cop.Transaction t
  pItems = map Cop.Price ps
  in case pItems of
    [] -> tItem :| []
    p1:pr -> p1 :| (pr ++ [tItem])

transactionItem :: Parser (NonEmpty Cop.Item)
transactionItem = transactionPairToItem <$> transaction

item :: Parser (NonEmpty Cop.Item)
item = (:|) <$> blankLine <*> pure []
       <|> (:|) <$> comment <*> pure []
       <|> transactionItem

items :: Parser [Cop.Item]
items = option []
        $ (concat . map toList)
        <$> many item

-- | Converts a Ledger to Penny data. /Warning:/ this parser was
-- designed to parse my ledger file from the Ledger 2.6 series. My
-- ledger file had only US-ASCII, so I paid no attention to making
-- sure this works with any characters above code point 127. Also, I
-- did not use all the features of Ledger. I used only unit prices,
-- not total prices, and I had no stand-alone prices specified in my
-- ledger (all prices were part of a posting). Thus this parser does
-- not parse these. I also do not use the time clock features or any
-- of the directives (for instance, the directives that let you
-- include one file from another). This makes no attempt to handle
-- those either.
-- 
-- Therefore this function does not truly provide Ledger
-- compatibility. Nevertheless it is here in case you find it
-- useful. To use this I wrote a simple Haskell script that parsed the
-- Ledger with this parser and then rendered it in native
-- "Penny.Copper" format using the functions in "Penny.Copper". This
-- parser preserves all comments and blank lines from th Ledger. If
-- you improve these parsers to broaden the Ledger compatibility, let
-- me know on Github and maybe I will incorporate your changes.
--
-- Ledger will allow transactions with prices to be unbalanced, even
-- if only by a small fraction. This is impossible in Penny, and this
-- parser makes no effort to deal with this. If a Ledger transaction
-- is unbalanced, this parser will simply fail with an error message.
--
-- Because Penny handles commodity prices completely differently from
-- Ledger, this parser bakes in some compensations. Essentially, a
-- posting with a price results in three Penny postings as well as a
-- standalone price. See the comments in "Penny.Denver.Transaction"
-- for more details on that.
ledger :: Parser [Cop.Item]
ledger = many (char ' ') *> items <* eof
