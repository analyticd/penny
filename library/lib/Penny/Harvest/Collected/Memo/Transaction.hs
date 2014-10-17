module Penny.Harvest.Collected.Memo.Transaction where

import qualified Penny.Harvest.Zoned.Located as Located
import qualified Penny.Tree.Memo.Transaction as Memo.Transaction
import Data.Sequence (Seq, (|>))
import qualified Penny.Core.Memo as Memo

-- | Currently in a TopLineMemo

newtype T = T { toSeq :: Seq (Located.T Memo.Transaction.T) }
  deriving (Eq, Ord, Show)

addMemo :: T -> Located.T Memo.Transaction.T -> T
addMemo (T sq) m = T (sq |> m)

toCore :: T -> Memo.T
toCore = Memo.T . fmap (Memo.Transaction.toCore . Located.payload)
  . toSeq
