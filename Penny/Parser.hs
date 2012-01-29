module Penny.Parser where

import Text.Parsec (
  try, (<|>), anyChar, string, manyTill, satisfy,
  char, notFollowedBy, many, skipMany, optional,
  option, digit, choice,
  optionMaybe, many1, Column, sourceColumn, sourceLine,
  getParserState, noneOf, statePos, Line, eof,
  parse )
import Text.Parsec.Text ( Parser )

import Data.Char ( isLetter, isNumber, isPunctuation, isSymbol )
import qualified Data.Char as Char
import Control.Monad ( void, liftM, replicateM, when )
import Data.Text ( pack )
import Data.Time.LocalTime ( TimeZone )
import Control.Applicative ((<*>), pure)

import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Foldable as F
import Data.Maybe ( catMaybes, isNothing )
import Text.Show.Pretty (ppShow)

import qualified Penny.Bits as B
import qualified Penny.Bits.DateTime as DT
import qualified Penny.Bits.Commodity as C
import qualified Penny.Bits.Amount as A
import qualified Penny.Bits.Price as Pr
import qualified Penny.Bits.PricePoint as PP
import Penny.TextNonEmpty ( TextNonEmpty ( TextNonEmpty ) )
import Penny.Groups.AtLeast1 ( AtLeast1 ( AtLeast1 ) )
import Penny.Groups.AtLeast2 ( AtLeast2 ( AtLeast2 ) )
import Penny.Bits.Qty ( Qty, partialNewQty )
import qualified Penny.Reports as R
import qualified Penny.Posting.Unverified.Parent as UPa
import qualified Penny.Posting.Unverified.Posting as UPo
import qualified Penny.Posting as P

import qualified Penny.Parser.Comments.SingleLine as CS
import qualified Penny.Parser.Comments.Multiline as CM
import qualified Penny.Parser.Amount as A
import qualified Penny.Parser.Commodity as C
import qualified Penny.Parser.Entry as E
import qualified Penny.Parser.Qty as Q

import Penny.Parser.DateTime (
  dateTime, DefaultTimeZone ( DefaultTimeZone ))

subAccountChar :: Parser Char
subAccountChar = let
  notSpc = satisfy (\l -> isLetter l || isNumber l)
  spc = do
    void $ char ' '
    notFollowedBy (char ' ' <|> char '#')
    return ' '
  in notSpc <|> try spc

subAccountName :: Parser B.SubAccountName
subAccountName = do
  c <- subAccountChar
  r <- liftM pack $ many subAccountChar
  return . B.SubAccountName $ TextNonEmpty c r

firstSubAccount :: Parser B.SubAccountName
firstSubAccount = subAccountName

nextSubAccount :: Parser B.SubAccountName
nextSubAccount = char ':' >> subAccountName

account :: Parser B.Account
account = do
  f <- firstSubAccount
  r <- many nextSubAccount
  return . B.Account $ AtLeast1 f r

tagChar :: Parser Char
tagChar = satisfy (\l -> isLetter l || isNumber l)

tag :: Parser B.TagName
tag = do
  _ <- char '#'
  f <- tagChar
  r <- liftM pack (many tagChar)
  return . B.TagName $ TextNonEmpty f r

firstTag :: Parser B.TagName
firstTag = tag

-- Do not use Parsec's "space" parser for spaces. It parses any
-- whitespace character, including tabs and newlines. Instead use char
-- ' '.

-- | Tags must be separated by at least one space. (Is this a good
-- restriction? Does not seem to be necessary.)
nextTag :: Parser B.TagName
nextTag =
  char ' '
  >> skipMany (char ' ')
  >> tag

tags :: Parser B.Tags
tags = do
  f <- firstTag
  rs <- many (try nextTag)
  return $ B.Tags (f : rs)

transactionPayee :: Parser B.Payee
transactionPayee = do
  c <- anyChar
  rs <- liftM pack (manyTill (noneOf "\n") (char '\n'))
  return . B.Payee $ TextNonEmpty c rs

data PriceData =
  PriceData { pricePoint :: PP.PricePoint
            , priceFormat :: (C.Commodity, R.CommodityFmt) }
  deriving Show

price ::
  DefaultTimeZone
  -> Q.Radix
  -> Q.Separator
  -> Parser PriceData
price dtz rad sep = do
  void $ char 'P'
  whitespace
  dt <- dateTime dtz
  whitespace
  com <- C.commoditySymbol <|> C.commodityLong
  whitespace
  (amt, pair) <- A.amount rad sep
  let (from, to) = (Pr.From com, Pr.To (A.commodity amt))
      cpu = Pr.CountPerUnit (A.qty amt)
  pr <- case Pr.price from to cpu of
    (Just pri) -> return pri
    Nothing -> fail "invalid price given"
  return $ PriceData (PP.PricePoint dt pr) pair
  

number :: Parser B.Number
number = do
  void $ char '('
  let p l =  isLetter l || isNumber l
  c <- satisfy p
  cs <- manyTill (satisfy p) (char ')')
  return . B.Number $ TextNonEmpty c (pack cs)

postingPayee :: Parser B.Payee
postingPayee = do
  void $ char '<'
  let p c = notElem c "<>" && (isLetter c || isNumber c
            || isPunctuation c || isSymbol c || c == ' ')
  c <- satisfy p
  cs <- manyTill (satisfy p) (char '>')
  return . B.Payee $ TextNonEmpty c (pack cs)

transactionMemoLine :: Parser String
transactionMemoLine = do
  void $ char ';'
  cs <- many1 (satisfy (/= '\n'))
  void $ char '\n'
  return (cs ++ "\n")

transactionMemo :: Parser B.Memo
transactionMemo = do
  (c:cs) <- liftM concat $ many1 transactionMemoLine
  return . B.Memo $ TextNonEmpty c (pack cs)

postingMemoLine ::
  PostingFirstColumn
  -- ^ Column that the posting line started at
  -> Parser String
postingMemoLine (PostingFirstColumn aboveCol) = do
  whitespace
  st <- getParserState
  let currCol = sourceColumn . statePos $ st
  when (currCol <= aboveCol) $
    fail $ "memo line is not indented farther than corresponding "
    ++ "posting line"
  c <- noneOf "\t\n"
  cs <- manyTill (noneOf "\t\n") (char '\n')
  return (c : (cs ++ "\n"))

postingMemo ::
  PostingFirstColumn
  -> Parser B.Memo
postingMemo col = do
  (c:cs) <- liftM concat (many1 (try (postingMemoLine col)))
  return . B.Memo $ TextNonEmpty c (pack cs)

flag :: Parser B.Flag
flag = do
  void $ char '['
  c <- anyChar
  void $ char ']'
  return $ B.Flag c

whitespace :: Parser ()
whitespace = void (many (char ' '))

parent :: DefaultTimeZone -> Parser UPa.Parent
parent dtz = do
  m <- optionMaybe transactionMemo
  d <- dateTime dtz
  whitespace
  f <- optionMaybe flag
  whitespace
  n <- optionMaybe number
  whitespace
  p <- optionMaybe transactionPayee
  when (isNothing p) (void $ char '\n')
  return $ UPa.Parent d f n p m

data PostingFirstColumn = PostingFirstColumn Column
                          deriving Show
data PostingLine = PostingLine Line
                   deriving Show

data PostingData =
  PostingData { firstColumn :: PostingFirstColumn
              , line :: PostingLine
              , unverified :: UPo.Posting
              , commodity :: Maybe C.Commodity
              , format :: Maybe R.CommodityFmt }
  deriving Show

posting :: Q.Radix -> Q.Separator -> Parser PostingData
posting rad sep = do
  void $ char ' '
  whitespace
  st <- getParserState
  let col = PostingFirstColumn . sourceColumn . statePos $ st
      lin = PostingLine . sourceLine . statePos $ st
  f <- optionMaybe flag
  whitespace
  n <- optionMaybe number
  whitespace
  p <- optionMaybe postingPayee
  whitespace
  a <- account
  whitespace
  t <- option (B.Tags []) tags
  whitespace
  (e, c, fmt) <- do
    me <- optionMaybe $ E.entry rad sep
    case me of
      (Just (e', (c', fmt'))) -> return (Just e', Just c', Just fmt')
      Nothing -> return (Nothing, Nothing, Nothing)
  whitespace
  void $ char '\n'
  m <- optionMaybe $ try (postingMemo col)
  let pd = PostingData col lin unv c fmt
      unv = UPo.Posting p n f a e t m
  return pd

data TransactionData =
  TransactionData { transaction :: P.Transaction
                  , postingLines :: AtLeast2 PostingLine
                  , formats :: [(C.Commodity, R.CommodityFmt)] }
  deriving Show

transactionParser ::
  DefaultTimeZone
  -> Q.Radix
  -> Q.Separator
  -> Parser TransactionData
transactionParser dtz rad sep = do
  pa <- parent dtz
  p1 <- posting rad sep
  p2 <- posting rad sep
  ps <- many (try (posting rad sep))
  let a2 = AtLeast2 p1 p2 ps
      errXact = P.transaction pa (fmap unverified a2)
  xact <- case errXact of
    (Ex.Exception err) -> fail $ errorStr err
    (Ex.Success x) -> return x
  let lns = fmap line a2
      fmtPairs = zip cs fs
      cs = catMaybes . F.toList . fmap commodity $ a2
      fs = catMaybes . F.toList . fmap format $ a2
  return $ TransactionData xact lns fmtPairs

errorStr :: P.Error -> String
errorStr e = case e of
  P.UnbalancedError -> "postings are not balanced"
  P.TooManyInferError -> "too many postings with entry amounts to infer"
  P.CouldNotInferError -> "could not infer entry for posting"

data Item = Transaction TransactionData
          | Price PriceData
          | Multiline CM.Multiline
          | SingleLine CS.Comment
          | BlankLine
          deriving Show

data ItemLineNumber = ItemLineNumber Line
                      deriving Show

data ItemWithLineNumber =
  ItemWithLineNumber { item :: Item
                     , lineNumber :: ItemLineNumber }
  deriving Show

itemWithLineNumber ::
  DefaultTimeZone
  -> Q.Radix
  -> Q.Separator
  -> Parser ItemWithLineNumber
itemWithLineNumber dtz rad sep = do
  st <- getParserState
  let currLine = sourceLine . statePos $ st
  i <- parseItem dtz rad sep
  return $ ItemWithLineNumber i (ItemLineNumber currLine)

parseItem ::
  DefaultTimeZone
  -> Q.Radix
  -> Q.Separator
  -> Parser Item
parseItem dtz rad sep = let
   bl = char '\n' >> return BlankLine
   t = liftM Transaction $ transactionParser dtz rad sep
   p = liftM Price $ price dtz rad sep
   cm = liftM Multiline CM.multiline
   co = liftM SingleLine CS.comment
   in (bl <|> t <|> p <|> cm <|> co)

ledger ::
  DefaultTimeZone
  -> Q.Radix
  -> Q.Separator
  -> Parser [ItemWithLineNumber]
ledger dtz rad sep = manyTill (itemWithLineNumber dtz rad sep) eof
