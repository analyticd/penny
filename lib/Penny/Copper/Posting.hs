module Penny.Copper.Posting (
  posting, render,
  UnverifiedWithMeta(
    UnverifiedWithMeta, flag, number, payee,
    account, tags, entry, memo),
  unverifiedWithMeta
  ) where

import Control.Applicative ((<$>), (<*>), (<*), (<|>))
import qualified Data.Text as X
import Text.Parsec (
  getParserState,
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
import Penny.Copper.Util (lexeme, eol, renMaybe, txtWords)
import qualified Penny.Lincoln.Meta as M
import qualified Penny.Lincoln.Transaction.Unverified as U

posting :: Qt.RadGroup
           -> Parser (U.Posting, M.PostingMeta)
posting rg =
  makeUnverified
  <$> (M.PostingLine . M.Line . sourceLine . statePos
       <$> getParserState)
  <*> (UnverifiedWithMeta
       <$> optionMaybe (lexeme Fl.flag)
       <*> optionMaybe (lexeme Nu.number)
       <*> optionMaybe (lexeme Pa.quotedPayee)
       <*> lexeme (Ac.lvl1AccountQuoted <|> Ac.lvl2Account)
       <*> lexeme Ta.tags
       <*> optionMaybe (lexeme (En.entry rg))
       <* eol
       <*> Me.memo)
  <?> "posting"

data UnverifiedWithMeta = UnverifiedWithMeta {
  flag :: Maybe B.Flag
  , number :: Maybe B.Number
  , payee :: Maybe B.Payee
  , account :: B.Account
  , tags :: B.Tags
  , entry :: Maybe (B.Entry, M.Format)
  , memo :: B.Memo
  } deriving (Eq, Show)
    
unverifiedWithMeta ::
  (U.Posting, M.PostingMeta)
  -> Maybe UnverifiedWithMeta
unverifiedWithMeta (upo, meta) =
  let (U.Posting pa nu fl ac ta mayEn me) = upo in
  case (mayEn, M.postingFormat meta) of
    (Nothing, Nothing) -> Just UnverifiedWithMeta {
      flag = fl
      , number = nu
      , payee = pa
      , account = ac
      , tags = ta
      , entry = Nothing
      , memo = me }
    (Just en, Just mt) -> Just UnverifiedWithMeta {
      flag = fl
      , number = nu
      , payee = pa
      , account = ac
      , tags = ta
      , entry = Just (en, mt)
      , memo = me }
    _ -> Nothing

      

makeUnverified ::
  M.PostingLine
  -> UnverifiedWithMeta
  -> (U.Posting, M.PostingMeta)
makeUnverified pl u = (upo, meta) where
  upo = U.Posting (payee u) (number u) (flag u) (account u)
        (tags u) en (memo u)
  meta = M.PostingMeta (Just pl) fmt
  (en, fmt) = case entry u of
    Nothing -> (Nothing, Nothing)
    Just (e, f) -> (Just e, Just f)

-- | Renders an unverified Posting. Fails if any of the components
-- fail to render. In addition, if the unverified Posting has an
-- Entry, a Format must be provided, otherwise render fails.
render ::
  (Qt.GroupingSpec, Qt.GroupingSpec)
  -> Qt.RadGroup
  -> UnverifiedWithMeta
  -> Maybe X.Text
render (gl, gr) rg u =
  f
  <$> renMaybe (flag u) Fl.render
  <*> renMaybe (number u) Nu.render
  <*> renMaybe (payee u) Pa.quoteRender
  <*> Ac.render (account u)
  <*> Ta.render (tags u)
  <*> renMaybe (entry u) renderEn
  <*> Me.render (memo u)
  where
    f flX nuX paX acX taX enX meX = let
      ws = txtWords [flX, nuX, paX, acX, taX, enX]
      in X.pack (replicate 4 ' ')
         `X.append` ws
         `X.snoc` '\n'
         `X.append` meX
    renderEn (en, fmt) = En.render gl gr rg fmt en
