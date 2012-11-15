module Penny.Copper.Posting (
  posting, render
  ) where

import Control.Applicative ((<$>), (<*>), (<*), (<|>))
import qualified Data.Text as X
import Text.Parsec (
  getParserState,
  statePos, optionMaybe, sourceLine, (<?>),
  State)
import Text.Parsec.Text ( Parser )

import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Copper.Account as Ac
import qualified Penny.Copper.Entry as En
import qualified Penny.Copper.Flag as Fl
import qualified Penny.Copper.Memos.Posting as Me
import qualified Penny.Copper.Number as Nu
import qualified Penny.Copper.Payees as Pa
import qualified Penny.Copper.Qty as Qt
import qualified Penny.Copper.Tags as Ta
import Penny.Copper.Util (lexeme, eol, renMaybe, txtWords)
import qualified Penny.Lincoln.Transaction as T
import qualified Penny.Lincoln.Transaction.Unverified as U

posting :: Parser U.Posting
posting =
  makeUnverified
  <$> (L.PostingLine . sourceLine . statePos
       <$> getParserState)
  <*> (UnverifiedWithMeta
       <$> optionMaybe (lexeme Fl.flag)
       <*> optionMaybe (lexeme Nu.number)
       <*> optionMaybe (lexeme Pa.quotedPayee)
       <*> lexeme (Ac.lvl1AccountQuoted <|> Ac.lvl2Account)
       <*> lexeme Ta.tags
       <*> optionMaybe (lexeme En.entry)
       <* eol
       <*> Me.memo)
  <?> "posting"

data UnverifiedWithMeta = UnverifiedWithMeta {
  flag :: Maybe B.Flag
  , number :: Maybe B.Number
  , payee :: Maybe B.Payee
  , account :: B.Account
  , tags :: B.Tags
  , entry :: Maybe (B.Entry, L.Format)
  , memo :: B.Memo
  } deriving (Eq, Show)
    
makeUnverified ::
  L.PostingLine
  -> UnverifiedWithMeta
  -> U.Posting
makeUnverified pl u = upo where
  upo = U.Posting (payee u) (number u) (flag u) (account u)
        (tags u) en (memo u) meta
  (en, fmt) = case entry u of
    Nothing -> (Nothing, Nothing)
    Just (e, f) -> (Just e, Just f)
  meta = L.PostingMeta (Just pl) fmt Nothing Nothing


