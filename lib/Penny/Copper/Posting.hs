module Penny.Copper.Posting (
  posting, render,
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
import qualified Penny.Lincoln.Transaction as T
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


-- | Renders a Posting. Fails if any of the components
-- fail to render. In addition, if the unverified Posting has an
-- Entry, a Format must be provided, otherwise render fails.
render ::
  (Qt.GroupingSpec, Qt.GroupingSpec)
  -> Qt.RadGroup
  -> (T.Posting, M.PostingMeta)
  -> Maybe X.Text
render (gl, gr) rg (p, m) = do
  fl <- renMaybe (T.pFlag p) Fl.render
  nu <- renMaybe (T.pNumber p) Nu.render
  pa <- renMaybe (T.pPayee p) Pa.quoteRender
  ac <- Ac.render (T.pAccount p)
  ta <- Ta.render (T.pTags p)
  me <- Me.render (T.pMemo p)
  maybePair <- case (T.pInferred p, M.postingFormat m) of
    (T.Inferred, Nothing) -> return Nothing
    (T.NotInferred, Just f) -> return (Just (T.pEntry p, f))
    _ -> Nothing
  let renderEn (e, f) = En.render gl gr rg f e
  en <- renMaybe maybePair renderEn
  let ws = txtWords [fl, nu, pa, ac, ta, en]
  return $ X.pack (replicate 4 ' ')
    `X.append` ws
    `X.snoc` '\n'
    `X.append` me
