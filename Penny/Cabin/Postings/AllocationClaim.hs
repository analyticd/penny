module Penny.Cabin.Postings.AllocationClaim where

import qualified Data.Array as A
import qualified Data.Text as X

import qualified Penny.Cabin.Row as R
import Penny.Cabin.Postings.Address (Col, Row)
import qualified Penny.Cabin.Postings.Address as Adr
import qualified Penny.Cabin.Postings.Grid as G
import qualified Penny.Cabin.Postings.Types as T
import qualified Penny.Cabin.Postings.Fields as F
import qualified Penny.Cabin.Postings.Options as O
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Lincoln.HasText as HT

type Arr = A.Array (Col, (T.VisibleNum, Row))
           (T.PostingInfo, Maybe R.Cell)

type Address = (Col, Row)

allocationClaim ::
  O.Options
  -> G.AllocationClaim Col Row
allocationClaim _ _ _ (_, Just c) = G.AcCell c

allocationClaim opts _ (col, (_, tr)) (p, _) = case (col, tr) of
  (Adr.Payee, Adr.Top) -> let
    w = case Q.payee . T.postingBox $ p of
      Nothing -> 0
      (Just pye) -> X.length . HT.text $ pye
    in if F.payee . O.fields $ opts
       then G.AcWidth w
       else G.AcCell R.zeroCell
  (Adr.Account, Adr.Top) -> let
    w = X.length . HT.text . HT.Delimited (X.singleton ':')
        . HT.textList . Q.account . T.postingBox $ p
    in if F.account . O.fields $ opts
       then G.AcWidth w
       else G.AcCell R.zeroCell
  (Adr.Multi, row) -> case row of
    Adr.Top -> error "allocationClaim: error 1: should not happen"
    _ -> G.AcOverrunning
  _ -> error "allocationClaim: error 2: should not happen"
