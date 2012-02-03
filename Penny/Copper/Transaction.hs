module Penny.Copper.Transaction where

import qualified Control.Monad.Exception.Synchronous as Ex
import Text.Parsec (try, many )
import Text.Parsec.Text ( Parser )

import Penny.Lincoln.Family.Family ( Family ( Family ) )
import qualified Penny.Copper.DateTime as DT
import Penny.Copper.TopLine ( topLine )
import qualified Penny.Copper.Posting as Po
import qualified Penny.Copper.Qty as Qt
import qualified Penny.Lincoln.Transaction as T
import qualified Penny.Copper.Meta as M

errorStr :: T.Error -> String
errorStr e = case e of
  T.UnbalancedError -> "postings are not balanced"
  T.TooManyInferError -> "too many postings with entry amounts to infer"
  T.CouldNotInferError -> "could not infer entry for posting"

transaction ::
  M.Filename
  -> DT.DefaultTimeZone
  -> Qt.Radix
  -> Qt.Separator
  -> Parser (T.Transaction, Family M.TransactionMeta M.PostingMeta)
transaction fn dtz rad sep = do
  (pa, tll, tml) <- topLine dtz
  let paMeta = M.TransactionMeta tml tll fn
  (p1, p1meta) <- Po.posting rad sep
  (p2, p2meta) <- Po.posting rad sep
  psPairs <- many (try (Po.posting rad sep))
  let (ps, psMeta) = (map fst psPairs, map snd psPairs)
      fam = Family pa p1 p2 ps
      errXact = T.transaction fam
  xact <- case errXact of
    (Ex.Exception err) -> fail $ errorStr err
    (Ex.Success x) -> return x
  return (xact, (Family paMeta p1meta p2meta psMeta))

