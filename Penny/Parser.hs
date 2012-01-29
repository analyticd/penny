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
import Penny.Parser.Account ( account )
import qualified Penny.Parser.Amount as A
import qualified Penny.Parser.Commodity as C
import qualified Penny.Parser.Entry as E
import qualified Penny.Parser.Qty as Q
import  Penny.Parser.Tags ( tags )
import Penny.Parser.Flag ( flag )

import Penny.Parser.DateTime (
  dateTime, DefaultTimeZone ( DefaultTimeZone ))




whitespace :: Parser ()
whitespace = void (many (char ' '))

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
