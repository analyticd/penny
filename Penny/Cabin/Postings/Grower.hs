module Penny.Cabin.Postings.Grower where

import Control.Applicative ((<$>), (<*>), pure)
import qualified Data.Array as A
import qualified Data.Foldable as F
import Data.Map ((!))
import qualified Data.Map as M
import Data.Maybe (isJust)
import qualified Data.Sequence as Seq
import qualified Data.Table as Tb
import qualified Data.Text as X

import qualified Penny.Lincoln.Balance as Bal
import qualified Penny.Lincoln.Meta as Me
import qualified Penny.Lincoln.Queries as Q
import Penny.Lincoln.HasText (text)
import qualified Penny.Lincoln.HasText as HT

import qualified Penny.Cabin.Colors as C
import qualified Penny.Cabin.Row as R
import Penny.Cabin.Postings.Address (Col, Row)
import qualified Penny.Cabin.Postings.Address as Adr
import qualified Penny.Cabin.Postings.Colors as PC
import qualified Penny.Cabin.Postings.Types as T
import qualified Penny.Cabin.Postings.Fields as F
import qualified Penny.Cabin.Postings.Options as O

type Arr = A.Array (Col, (T.VisibleNum, Row))
           (T.PostingInfo, Maybe T.ClaimedWidth)

type Grower =
  F.Fields Bool
  -> O.Options
  -> Arr
  -> (Col, (T.VisibleNum, Row))
  -> (T.PostingInfo, Maybe T.ClaimedWidth)
  -> Maybe R.Cell

widest :: Col -> Arr -> C.Width
widest col arr = F.foldr f (C.Width 0) clm where
  clm = Tb.OneDim . Tb.column arr $ col
  f c soFar = max width soFar where
    width = case snd c of
      Nothing -> C.Width 0
      (Just cw) ->
        C.Width
        . T.unClaimedWidth
        $ cw

padding :: Grower
padding _ opts a (col, (vn, _)) _ = let
  ts = PC.colors vn (O.baseColors opts)
  width = widest col a
  j = R.LeftJustify
  sq = Seq.empty
  in Just $ R.Cell j width ts sq

emptyButPadded :: Grower
emptyButPadded = padding

overran :: Grower
overran _ _ _ _ _ = Just R.zeroCell

allocated :: Grower
allocated _ _ _ _ _ = Nothing

overrunning :: Grower
overrunning _ _ _ _ _ = Nothing

lineNum :: Grower
lineNum flds 
