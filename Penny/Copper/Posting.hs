module Penny.Copper.Posting where

import Control.Monad ( void )
import Text.Parsec (
  char, many, getParserState, sourceColumn,
  statePos, optionMaybe, option, try, sourceLine )
import Text.Parsec.Text ( Parser )

import qualified Penny.Lincoln.Bits as B
import qualified Penny.Copper.Account as Ac
import qualified Penny.Copper.Entry as En
import qualified Penny.Copper.Flag as Fl
import qualified Penny.Copper.Memos.Posting as Me
import qualified Penny.Copper.Number as Nu
import qualified Penny.Copper.Payees.Posting as Pa
import qualified Penny.Copper.Qty as Qt
import qualified Penny.Copper.Tags as Ta
import qualified Penny.Copper.Meta as M
import qualified Penny.Lincoln.Transaction.Unverified as U

whitespace :: Parser ()
whitespace = void (many (char ' '))

posting :: Qt.Radix
           -> Qt.Separator
           -> Parser (U.Posting, M.PostingMeta)
posting rad sep = do
  void $ char ' '
  whitespace
  st <- getParserState
  let col = M.Column . sourceColumn . statePos $ st
      lin = M.PostingLine . M.Line . sourceLine . statePos $ st
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
  let unv = U.Posting p n f a t e m
  return (unv, M.PostingMeta lin maybeFmt)

