module Penny.Parser.Transaction where

import qualified Control.Monad.Exception.Synchronous as Ex
import Text.Parsec (try, many )
import Text.Parsec.Text ( Parser )

import Penny.Family.Family ( Family ( Family ) )
import qualified Penny.Parser.DateTime as DT
import Penny.Parser.Parent ( parent, ParentLine )
import qualified Penny.Parser.Posting as Po
import qualified Penny.Parser.Qty as Qt
import qualified Penny.Posting as P

errorStr :: P.Error -> String
errorStr e = case e of
  P.UnbalancedError -> "postings are not balanced"
  P.TooManyInferError -> "too many postings with entry amounts to infer"
  P.CouldNotInferError -> "could not infer entry for posting"

data Meta =
  Meta { unMeta :: Family ParentLine Po.Meta }
  deriving Show

transaction ::
  DT.DefaultTimeZone
  -> Qt.Radix
  -> Qt.Separator
  -> Parser (P.Transaction, Meta)
transaction dtz rad sep = do
  (pa, paMeta) <- parent dtz
  (p1, p1meta) <- Po.posting rad sep
  (p2, p2meta) <- Po.posting rad sep
  psPairs <- many (try (Po.posting rad sep))
  let (ps, psMeta) = (map fst psPairs, map snd psPairs)
      fam = Family pa p1 p2 ps
      errXact = P.transaction fam
  xact <- case errXact of
    (Ex.Exception err) -> fail $ errorStr err
    (Ex.Success x) -> return x
  return (xact, Meta (Family paMeta p1meta p2meta psMeta))

