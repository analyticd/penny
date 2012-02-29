module Penny.Cabin.Postings.AllocationClaim where

import qualified Penny.Cabin.Postings.Grid as G

import qualified Data.Array as A
import qualified Data.Foldable as F
import Data.List (intersperse)
import Data.Map ((!))
import qualified Data.Map as M
import Data.Monoid (mempty, mappend)
import qualified Data.Sequence as Seq
import qualified Data.Table as Tb
import qualified Data.Text as X

import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Lincoln.HasText as HT

import qualified Penny.Cabin.Allocate as Alo
import qualified Penny.Cabin.Colors as C
import qualified Penny.Cabin.Row as R
import qualified Penny.Cabin.TextFormat as TF
import Penny.Cabin.Postings.Address (Col, Row)
import qualified Penny.Cabin.Postings.Address as Adr
import qualified Penny.Cabin.Postings.Colors as PC
import qualified Penny.Cabin.Postings.Grid as G
import qualified Penny.Cabin.Postings.Types as T
import qualified Penny.Cabin.Postings.Fields as F
import qualified Penny.Cabin.Postings.Options as O

type Arr = A.Array (Col, (T.VisibleNum, Row))
           (T.PostingInfo, Maybe R.Cell)

type Address = (Col, Row)

allocationClaim :: G.AllocationClaim Col Row
allocationClaim _ _ (_, Just c) = G.AcCell c

allocationClaim a (col, (vn, tr)) (p, _) = case (col, tr) of
  (Adr.Payee, Adr.Top) -> let
    clm = Tb.column a col
    folder (info, mc) maxSoFar = case mc of
      Nothing -> maxSoFar
      (Just c) -> max maxSoFar w where
        w = case Q.payee . T.postingBox $ info of
          Nothing -> 0
          (Just pye) -> X.length . HT.text $ pye
    in G.AcWidth (F.foldr folder 0 clm)
  (Adr.Account, Adr.Top) -> let
    clm = Tb.column a col
    folder (info, mc) maxSoFar = case mc of
      Nothing -> maxSoFar
      (Just c) -> max maxSoFar w where
        w = X.length . HT.text . HT.Delimited (X.singleton ':')
            . HT.textList . Q.account . T.postingBox $ info
    in G.AcWidth (F.foldr folder 0 clm)
  (Adr.Multi, row) -> case row of
    Adr.Top -> error "allocationClaim: error 1: should not happen"
    _ -> G.AcOverrunning
  _ -> error "allocationClaim: error 2: should not happen"
