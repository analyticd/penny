module Penny.Copper.Transaction (transaction, render,
                                 boxToUnverifiedWithMeta) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (guard)
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Foldable (toList)
import qualified Data.Traversable as Tr
import qualified Data.Text as X
import Text.Parsec (many)
import Text.Parsec.Text ( Parser )

import qualified Penny.Copper.DateTime as DT
import qualified Penny.Copper.TopLine as TL
import Penny.Copper.TopLine ( topLine )
import qualified Penny.Copper.Posting as Po
import qualified Penny.Copper.Qty as Qt
import qualified Penny.Lincoln.Meta as M
import Penny.Lincoln.Family (orphans, marryWith, adopt)
import qualified Penny.Lincoln.Family.Family as F
import Penny.Lincoln.Family.Family ( Family ( Family ) )
import Penny.Lincoln.Meta (TransactionMeta(TransactionMeta))
import qualified Penny.Lincoln.Boxes as Boxes
import Penny.Lincoln.Boxes (transactionBox, TransactionBox)
import qualified Penny.Lincoln.Transaction as T
import qualified Penny.Lincoln.Transaction.Unverified as U

errorStr :: T.Error -> String
errorStr e = case e of
  T.UnbalancedError -> "postings are not balanced"
  T.CouldNotInferError -> "could not infer entry for posting"

mkTransaction ::
  M.Filename
  -> (U.TopLine, M.TopLineLine, M.TopMemoLine)
  -> (U.Posting, M.PostingMeta)
  -> (U.Posting, M.PostingMeta)
  -> [(U.Posting, M.PostingMeta)]
  -> Ex.Exceptional String TransactionBox
mkTransaction fn tripTop p1 p2 ps = let
  (tl, tll, tml) = tripTop
  famTrans = Family tl (fst p1) (fst p2) (map fst ps)
  paMeta = M.TopLineMeta (Just tml) (Just tll) (Just fn)
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
  mkTransaction fn
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

boxToUnverifiedWithMeta ::
  TransactionBox
  -> Maybe (Family U.TopLine Po.UnverifiedWithMeta)
boxToUnverifiedWithMeta b = do
  let t = T.unTransaction . Boxes.transaction $ b
  transMeta <- M.unTransactionMeta <$> Boxes.transactionMeta b
  guard (length (F.children transMeta) == length (F.children t))
  let famPairs = marryWith const (,) t transMeta
      pairs = fmap toUnverifiedPair $ orphans famPairs
      toUnverifiedPair (p, m) = (unverifyPosting p, m)
  unvsWithMetas <- Tr.traverse Po.unverifiedWithMeta pairs
  let uTopLine = unverifyTopLine (F.parent t)
  return $ adopt uTopLine unvsWithMetas

unverifyTopLine :: T.TopLine -> U.TopLine
unverifyTopLine t =
  U.TopLine (T.tDateTime t) (T.tFlag t)
  (T.tNumber t) (T.tPayee t) (T.tMemo t)

unverifyPosting :: T.Posting -> U.Posting
unverifyPosting p =
  U.Posting (T.pPayee p) (T.pNumber p) (T.pFlag p)
  (T.pAccount p) (T.pTags p) en (T.pMemo p)
  where
    en = case T.pInferred p of
      T.Inferred -> Just $ T.pEntry p
      T.NotInferred -> Nothing

render ::
  DT.DefaultTimeZone
  -> (Qt.GroupingSpec, Qt.GroupingSpec)
  -> Qt.RadGroup
  -> Family U.TopLine Po.UnverifiedWithMeta
  -> Maybe X.Text
render dtz gs rg fm = let
  pstgs = orphans fm
  tlX = TL.render dtz (F.parent fm)
  maybes = tlX : toList (fmap (Po.render gs rg) pstgs)
  in X.concat <$> sequence maybes
