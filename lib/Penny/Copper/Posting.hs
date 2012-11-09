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


-- | Renders a Posting. Fails if any of the components
-- fail to render. In addition, if the unverified Posting has an
-- Entry, a Format must be provided, otherwise render fails.
--
-- The columns look like this. Column numbers begin with 0 (like they
-- do in Emacs) rather than with column 1 (like they do in
-- Vim). (Really Emacs is the strange one; most CLI utilities seem to
-- start with column 1 too...)
--
-- > ID COLUMN WIDTH WHAT
-- > ---------------------------------------------------
-- > A    0      4     Blank spaces for indentation
-- > B    4      50    Flag, Number, Payee, Account, Tags
-- > C    54     2     Blank spaces for padding
-- > D    56     NA    Entry
--
-- Omit the padding after column B if there is no entry; also omit
-- columns C and D entirely if there is no Entry. (It is annoying to
-- have extraneous blank space in a file).
render ::
  (Qt.GroupingSpec, Qt.GroupingSpec)
  -> T.Posting
  -> Maybe X.Text
render gs p = do
  fl <- renMaybe (T.pFlag p) Fl.render
  nu <- renMaybe (T.pNumber p) Nu.render
  pa <- renMaybe (T.pPayee p) Pa.quoteRender
  ac <- Ac.render (T.pAccount p)
  ta <- Ta.render (T.pTags p)
  me <- Me.render (T.pMemo p)
  maybePair <- case (T.pInferred p, L.postingFormat . T.pMeta $ p) of
    (T.Inferred, Nothing) -> return Nothing
    (T.NotInferred, Just f) -> return (Just (T.pEntry p, f))
    _ -> Nothing
  let renderEn (e, f) = En.render gs f e
  en <- renMaybe maybePair renderEn
  return $ formatter fl nu pa ac ta en me

formatter ::
  X.Text    -- ^ Flag
  -> X.Text -- ^ Number
  -> X.Text -- ^ Payee
  -> X.Text -- ^ Account
  -> X.Text -- ^ Tags
  -> X.Text -- ^ Entry
  -> X.Text -- ^ Memo
  -> X.Text
formatter fl nu pa ac ta en me = let
  colA = X.pack (replicate 4 ' ')
  colBnoPad = txtWords [fl, nu, pa, ac, ta]
  colD = en
  colB = if X.null en
         then colBnoPad
         else X.justifyLeft 50 ' ' colBnoPad
  colC = if X.null en
         then X.empty
         else X.pack (replicate 2 ' ')
  rtn = X.singleton '\n'
  in X.concat [colA, colB, colC, colD, rtn, me]
