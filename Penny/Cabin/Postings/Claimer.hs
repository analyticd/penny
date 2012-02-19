module Penny.Cabin.Postings.Claimer where

import Control.Applicative ((<$>), (<*>), pure)
import qualified Data.Array as A
import Data.Map ((!))
import qualified Data.Map as M
import Data.Maybe (isJust)
import qualified Data.Text as X

import qualified Penny.Lincoln.Boxes as B
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Lincoln.Meta as Me

import Penny.Cabin.Postings.Address (Col, Row)
import qualified Penny.Cabin.Postings.Address as Adr
import qualified Penny.Cabin.Postings.Grid as G
import qualified Penny.Cabin.Postings.Types as T
import qualified Penny.Cabin.Postings.Fields as F
import qualified Penny.Cabin.Postings.Options as O

ifShown ::
  (F.Fields Bool -> Bool)
  -> F.Fields Bool
  -> Maybe T.ClaimedWidth
  -> Maybe T.ClaimedWidth
ifShown fn flds mt =
  if fn flds
  then mt
  else Nothing
 
type Claimer = 
  F.Fields Bool
  -> O.Options
  -> A.Array (Col, (T.VisibleNum, Row)) T.PostingInfo
  -> (Col, (T.VisibleNum, Row))
  -> T.PostingInfo
  -> Maybe T.ClaimedWidth

claimer :: Claimer
claimer flds opts a (col, (vn, r)) p = let
  f = claimLookup ! (col, r) in
  f flds opts a (col, (vn, r)) p

claimLookup :: M.Map (Col, Row) Claimer
claimLookup = foldl (flip . uncurry $ M.insert) noClaims ls where
  noClaims = M.fromList
    $ (,)
    <$> A.range ((minBound, minBound), (maxBound, maxBound))
    <*> pure noClaim
  ls = [lineNumTop, sLineNumTop,
        dateTop, sDateTop]

noClaim :: Claimer
noClaim _ _ _ _ _ = Nothing

claimOne :: Maybe T.ClaimedWidth
claimOne = Just $ T.ClaimedWidth 1

claimOneIf :: Bool -> Maybe T.ClaimedWidth
claimOneIf b =
  if b
  then claimOne
  else Nothing

lineNumTop :: ((Col, Row), Claimer)
lineNumTop = ((Adr.LineNum, Adr.Top), f) where
  f flds _ _ _ p =
    ifShown F.lineNum flds $
    case Q.postingLine . T.postingBox $ p of
      Nothing -> Nothing
      (Just n) ->
        Just
        . T.ClaimedWidth
        . length
        . show
        . Me.unLine
        . Me.unPostingLine
        $ n

sLineNumTop :: ((Col, Row), Claimer)
sLineNumTop = ((Adr.SLineNum, Adr.Top), f) where
  f flds _ _ _ p =
    ifShown F.lineNum flds
    . claimOneIf
    . isJust
    . Q.postingLine
    . T.postingBox
    $ p

dateTop :: ((Col, Row), Claimer)
dateTop = ((Adr.Date, Adr.Top), f) where
  f flds opts _ _ p =
    ifShown F.date flds $
    Just
    . T.ClaimedWidth
    . X.length
    . O.dateFormat opts
    . Q.dateTime
    . T.postingBox
    $ p

sDateTop :: ((Col, Row), Claimer)
sDateTop = ((Adr.SDate, Adr.Top), f) where
  f flds _ _ _ _ = ifShown F.date flds claimOne

{-
flag :: ((Col, Row), Claimer)
flag = ((Adr.Multi, Adr.Top), f) where
  f flds _ _ _ p =
    ifShown F.flag flds $
    case Q.flag . T.postingBox $ p of
      Nothing -> Nothing
      (Just flag) ->
        Just
        . T.ClaimedWidth
        . X.length
        . -}

-- Need to move HasText out of Predicates, then add another phase to
-- the Grid, to allow overrunning cells to come in last.
