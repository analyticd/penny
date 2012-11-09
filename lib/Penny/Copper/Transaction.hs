module Penny.Copper.Transaction (transaction, render) where

import Control.Applicative ((<$>), (<*>))
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.Foldable (toList)
import qualified Data.Traversable as Tr
import qualified Data.Text as X
import Text.Parsec (many)
import Text.Parsec.Text ( Parser )

import qualified Penny.Copper.TopLine as TL
import Penny.Copper.TopLine ( topLine )
import qualified Penny.Copper.Posting as Po
import qualified Penny.Copper.Qty as Qt
import qualified Penny.Lincoln as L
import Penny.Lincoln.Family (orphans)
import qualified Penny.Lincoln.Family.Family as F
import Penny.Lincoln.Family.Family ( Family ( Family ) )
import qualified Penny.Lincoln.Transaction as T
import qualified Penny.Lincoln.Transaction.Unverified as U

errorStr :: T.Error -> String
errorStr e = case e of
  T.UnbalancedError -> "postings are not balanced"
  T.CouldNotInferError -> "could not infer entry for posting"

mkTransaction ::
  U.TopLine
  -> U.Posting
  -> U.Posting
  -> [U.Posting]
  -> Ex.Exceptional String L.Transaction
mkTransaction top p1 p2 ps = let
  famTrans = Family top p1 p2 ps
  errXact = T.transaction famTrans
  in case errXact of
    Ex.Exception err -> Ex.Exception . errorStr $ err
    Ex.Success x -> return x

maybeTransaction :: Parser (Ex.Exceptional String L.Transaction)
maybeTransaction =
  mkTransaction
  <$> topLine
  <*> Po.posting
  <*> Po.posting
  <*> many Po.posting

transaction :: Parser L.Transaction
transaction = do
  ex <- maybeTransaction
  case ex of
    Ex.Exception s -> fail s
    Ex.Success b -> return b

render ::
  (Qt.GroupingSpec, Qt.GroupingSpec)
  -> T.Transaction
  -> Maybe X.Text
render gs txn = do
  let txnFam = T.unTransaction txn
  tlX <- TL.render (F.parent txnFam)
  pstgsX <- Tr.traverse (Po.render gs) (orphans txnFam)
  return $ tlX `X.append` (X.concat (toList pstgsX))



