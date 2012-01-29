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
