module Penny.Copper.Posting where

import Control.Applicative ((<$>), (<*>), (<*), (<|>),
                            (<**>), pure)
import Text.Parsec (
  char, many, getParserState,
  statePos, optionMaybe, sourceLine, (<?>),
  State)
import Text.Parsec.Text ( Parser )

import qualified Penny.Lincoln.Bits as B
import qualified Penny.Copper.Account as Ac
import qualified Penny.Copper.Entry as En
import qualified Penny.Copper.Flag as Fl
import qualified Penny.Copper.Memos.Posting as Me
import qualified Penny.Copper.Number as Nu
import qualified Penny.Copper.Payees as Pa
import qualified Penny.Copper.Qty as Qt
import qualified Penny.Copper.Tags as Ta
import Penny.Copper.Util (lexeme, eol)
import qualified Penny.Lincoln.Meta as M
import qualified Penny.Lincoln.Transaction.Unverified as U

posting :: Qt.RadGroup
           -> Parser (U.Posting, M.PostingMeta)
posting rg =
  makeUnverified
  <$> (M.PostingLine . M.Line . sourceLine . statePos
       <$> getParserState)
  <*> optionMaybe (lexeme Fl.flag)
  <*> optionMaybe (lexeme Nu.number)
  <*> optionMaybe (lexeme Pa.quotedPayee)
  <*> lexeme (Ac.lvl1AccountQuoted <|> Ac.lvl2Account)
  <*> lexeme Ta.tags
  <*> optionMaybe (lexeme (En.entry rg))
  <* eol
  <*> Me.memo
  <?> "posting"

makeUnverified ::
  M.PostingLine
  -> Maybe B.Flag
  -> Maybe B.Number
  -> Maybe B.Payee
  -> B.Account
  -> B.Tags
  -> Maybe (B.Entry, M.Format)
  -> B.Memo
  -> (U.Posting, M.PostingMeta)
makeUnverified pl fl nu pa ac ta pair me = (upo, meta) where
  upo = U.Posting pa nu fl ac ta en me
  meta = M.PostingMeta (Just pl) fmt
  (en, fmt) = case pair of
    Nothing -> (Nothing, Nothing)
    Just (e, f) -> (Just e, Just f)
