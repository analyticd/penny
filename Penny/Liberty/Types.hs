module Penny.Liberty.Types where

import Penny.Lincoln.Boxes (PostingBox)
import Penny.Lincoln.Predicates as P

data PostingInfo =
  PostingInfo { postingBox :: PostingBox
              , fwdSeqUnsorted :: FwdSeqUnsorted
              , backSeqUnsorted :: BackSeqUnsorted
              , fwdSeqSorted :: FwdSeqSorted
              , backSeqSorted :: BackSeqSorted }

newtype FwdSeqSorted = FwdSeqSorted { unFwdSeqSorted :: Integer }
                       deriving (Show, Eq, Ord)

newtype BackSeqSorted = BackSeqSorted { unBackSeqSorted :: Integer }
                      deriving (Show, Eq, Ord)

newtype FwdSeqUnsorted = FwdSeqUnsorted { unFwdSeqUnsorted :: Integer }
                       deriving (Show, Eq, Ord)

newtype BackSeqUnsorted = BackSeqUnsorted { unBackSeqUnsorted :: Integer }
                        deriving (Show, Eq, Ord)

testFwdSeqSorted ::
  P.Comparer -> FwdSeqSorted -> PostingInfo -> Bool
testFwdSeqSorted c f = P.comp c f fwdSeqSorted

testFwdSeqUnsorted ::
  P.Comparer -> FwdSeqUnsorted -> PostingInfo -> Bool
testFwdSeqUnsorted c f = P.comp c f fwdSeqUnsorted

testBackSeqUnsorted ::
  P.Comparer -> BackSeqUnsorted -> PostingInfo -> Bool
testBackSeqUnsorted c f = P.comp c f backSeqUnsorted

testBackSeqSorted ::
  P.Comparer -> BackSeqSorted -> PostingInfo -> Bool
testBackSeqSorted c f = P.comp c f backSeqSorted

