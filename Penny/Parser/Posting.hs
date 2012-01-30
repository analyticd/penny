module Penny.Parser.Posting where

import Control.Monad ( void )
import Text.Parsec (
  char, Line, many, getParserState, sourceColumn,
  statePos, optionMaybe, option, try, sourceLine )
                     
import Text.Parsec.Text ( Parser )

import qualified Penny.Bits as B
import qualified Penny.Bits.Commodity as C
import qualified Penny.Parser.Account as Ac
import qualified Penny.Parser.Entry as En
import qualified Penny.Parser.Flag as Fl
import qualified Penny.Parser.Memos.Posting as Me
import qualified Penny.Parser.Number as Nu
import qualified Penny.Parser.Payees.Posting as Pa
import qualified Penny.Parser.Qty as Qt
import qualified Penny.Parser.Tags as Ta
import qualified Penny.Posting.Unverified.Posting as UPo
import qualified Penny.Reports as R

data PostingLine = PostingLine Line
                   deriving Show

data Meta =
  Meta { firstColumn :: Me.PostingFirstColumn
       , line :: PostingLine
       , comFmt :: Maybe (C.Commodity, R.CommodityFmt) }
  deriving Show

whitespace :: Parser ()
whitespace = void (many (char ' '))

posting :: Qt.Radix -> Qt.Separator -> Parser (UPo.Posting, Meta)
posting rad sep = do
  void $ char ' '
  whitespace
  st <- getParserState
  let col = Me.PostingFirstColumn . sourceColumn . statePos $ st
      lin = PostingLine . sourceLine . statePos $ st
  f <- optionMaybe Fl.flag
  whitespace
  n <- optionMaybe Nu.number
  whitespace
  p <- optionMaybe Pa.payee
  whitespace
  a <- Ac.account
  whitespace
  t <- option (B.Tags []) Ta.tags
  whitespace
  (e, maybeFmt) <- do
    me <- optionMaybe $ En.entry rad sep
    case me of
      (Just (e', fmt')) -> return (Just e', Just fmt')
      Nothing -> return (Nothing, Nothing)
  whitespace
  void $ char '\n'
  m <- optionMaybe $ try (Me.memo col)
  let meta = Meta col lin maybeFmt
      unv = UPo.Posting p n f a e t m
  return (unv, meta)

