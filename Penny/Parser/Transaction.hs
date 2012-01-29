module Penny.Parser.Transaction where

import Control.Monad ( void )
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Foldable as F
import Data.Maybe ( catMaybes )
import Text.Parsec (
  char, Line, many1, getParserState, sourceColumn,
  statePos, optionMaybe, option, try, sourceLine,
  many )
import Text.Parsec.Text ( Parser )

import qualified Penny.Bits as B
import qualified Penny.Bits.Commodity as C
import Penny.Groups.AtLeast2 ( AtLeast2 ( AtLeast2 ) )
import qualified Penny.Parser.Account as Ac
import qualified Penny.Parser.Amount as Am
import qualified Penny.Parser.DateTime as DT
import qualified Penny.Parser.Entry as En
import qualified Penny.Parser.Flag as Fl
import qualified Penny.Parser.Memos.Posting as Me
import qualified Penny.Parser.Number as Nu
import qualified Penny.Parser.Parent as Pa
import qualified Penny.Parser.Payees.Posting as Pa
import qualified Penny.Parser.Posting as Po
import qualified Penny.Parser.Qty as Qt
import qualified Penny.Parser.Tags as Ta
import qualified Penny.Posting as P
import qualified Penny.Parser.Transaction.Data as Da
import qualified Penny.Posting.Unverified.Posting as UPo
import qualified Penny.Reports as R

errorStr :: P.Error -> String
errorStr e = case e of
  P.UnbalancedError -> "postings are not balanced"
  P.TooManyInferError -> "too many postings with entry amounts to infer"
  P.CouldNotInferError -> "could not infer entry for posting"

transactionParser ::
  DT.DefaultTimeZone
  -> Qt.Radix
  -> Qt.Separator
  -> Parser Da.TransactionData
transactionParser dtz rad sep = do
  pa <- Pa.parent dtz
  p1 <- Po.posting rad sep
  p2 <- Po.posting rad sep
  ps <- many (try (Po.posting rad sep))
  let a2 = AtLeast2 p1 p2 ps
      errXact = P.transaction pa (fmap Po.unverified a2)
  xact <- case errXact of
    (Ex.Exception err) -> fail $ errorStr err
    (Ex.Success x) -> return x
  let lns = fmap Po.line a2
      fmtPairs = zip cs fs
      cs = catMaybes . F.toList . fmap Po.commodity $ a2
      fs = catMaybes . F.toList . fmap Po.format $ a2
  return $ Da.TransactionData xact lns fmtPairs

