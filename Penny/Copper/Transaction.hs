module Penny.Copper.Transaction where

import Control.Applicative ((<$>), (<*>))
import qualified Control.Monad.Exception.Synchronous as Ex
import Text.Parsec (try, many )
import Text.Parsec.Text ( Parser )

import qualified Penny.Copper.DateTime as DT
import Penny.Copper.TopLine ( topLine )
import qualified Penny.Copper.Posting as Po
import qualified Penny.Copper.Qty as Qt
import qualified Penny.Lincoln.Meta as M
import Penny.Lincoln.Family.Family ( Family ( Family ) )
import Penny.Lincoln.Meta (TransactionMeta(TransactionMeta))
import Penny.Lincoln.Boxes (transactionBox, TransactionBox)
import qualified Penny.Lincoln.Transaction as T
import qualified Penny.Lincoln.Transaction.Unverified as U

errorStr :: T.Error -> String
errorStr e = case e of
  T.UnbalancedError -> "postings are not balanced"
  T.TooManyInferError -> "too many postings with entry amounts to infer"
  T.CouldNotInferError -> "could not infer entry for posting"

mkTransaction ::
  M.Filename
  -> DT.DefaultTimeZone
  -> Qt.RadGroup
  -> (U.TopLine, M.TopLineLine, Maybe M.TopMemoLine)
  -> (U.Posting, M.PostingMeta)
  -> (U.Posting, M.PostingMeta)
  -> [(U.Posting, M.PostingMeta)]
  -> Ex.Exceptional String TransactionBox
mkTransaction fn dtz rg tripTop p1 p2 ps = let
  (tl, tll, tml) = tripTop
  famTrans = Family tl (fst p1) (fst p2) (map fst ps)
  paMeta = M.TopLineMeta tml (Just tll) (Just fn)
  famMeta = Family paMeta (snd p1) (snd p2) (map snd ps)
  meta = Just . TransactionMeta $ famMeta
  errXact = T.transaction famTrans
  in case errXact of
    Ex.Exception err -> Ex.Exception . errorStr $ err
    Ex.Success x -> return (transactionBox x meta)

maybeTransaction ::
  M.Filename
  -> DT.DefaultTimeZone
  -> Qt.RadGroup
  -> Parser (Ex.Exceptional String TransactionBox)
maybeTransaction fn dtz rg =
  mkTransaction fn dtz rg
  <$> topLine dtz
  <*> Po.posting rg
  <*> Po.posting rg
  <*> many (Po.posting rg)

transaction ::
  M.Filename
  -> DT.DefaultTimeZone
  -> Qt.RadGroup
  -> Parser TransactionBox
transaction fn dtz rg = do
  ex <- maybeTransaction fn dtz rg
  case ex of
    Ex.Exception s -> fail s
    Ex.Success b -> return b
