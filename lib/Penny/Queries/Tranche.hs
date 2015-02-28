-- | Queries on 'Tranche's.

module Penny.Queries.Tranche where

import qualified Penny.Lincoln as L
import Data.Sequence (Seq)

trancheSerset :: L.Tranche l a -> L.Serset
trancheSerset (L.Tranche _ s) = s

splint :: L.Tranche l a -> L.Splint l a
splint (L.Tranche l _) = l

splintSerset :: L.Tranche l a -> L.Serset
splintSerset (L.Tranche (L.Splint _ s _) _) = s

balances :: L.Tranche l a -> L.Balances
balances (L.Tranche (L.Splint _ _ b) _) = b

sliceSerset :: L.Tranche l a -> L.Serset
sliceSerset (L.Tranche (L.Splint (L.Slice _ s _ _) _ _ ) _) = s

qty :: L.Tranche l a -> L.Qty
qty (L.Tranche (L.Splint (L.Slice _ _ q _) _ _) _) = q

commodity :: L.Tranche l a -> L.Commodity
commodity (L.Tranche (L.Splint (L.Slice _ _ _ cy) _ _) _) = cy

clatch :: L.Tranche l a -> L.Clatch l a
clatch (L.Tranche (L.Splint (L.Slice cl _ _ _) _ _) _) = cl

transaction :: L.Tranche l a -> L.TransactionL l
transaction t = let L.Clatch tx _ _ _ = clatch t in tx

leftSiblings :: L.Tranche l a -> Seq (L.PostingL l, a)
leftSiblings t = let L.Clatch _ x _ _ = clatch t in x

posting :: L.Tranche l a -> (L.PostingL l, a)
posting t = let L.Clatch _ _ x _ = clatch t in x

rightSiblings :: L.Tranche l a -> Seq (L.PostingL l, a)
rightSiblings t = let L.Clatch _ _ _ x = clatch t in x

transactionMeta :: L.Ledger l => L.Tranche l a -> l (Seq (L.TreeL l))
transactionMeta = L.transactionMeta . transaction

topLineSerial :: L.Ledger l => L.Tranche l a -> l L.TopLineSer
topLineSerial = L.topLineSerial . transaction

postingTrees :: L.Ledger l => L.Tranche l a -> l (Seq (L.TreeL l))
postingTrees = L.postingTrees . fst . posting

trio :: L.Ledger l => L.Tranche l a -> l L.Trio
trio = L.postingTrio . fst . posting

postingSer :: L.Ledger l => L.Tranche l a -> l L.PostingSer
postingSer = L.postingSerial . fst . posting

-- | Looks for a tree matching a predicate in the posting's trees.  If
-- one is not found, looks in the transaction's trees.
best
  :: L.Ledger l
  => (L.Realm -> L.Scalar -> Bool)
  -> L.Tranche l a
  -> l (Maybe (L.TreeL l))
best pd tr = do
  pts <- postingTrees tr
  mayTree <- L.findTreeInForest pd pts
  case mayTree of
    Nothing -> do
      tts <- transactionMeta tr
      L.findTreeInForest pd tts
    x -> return x

side :: L.Tranche l a -> Maybe L.Side
side = L.qtySide . qty
