module Penny.Harvest.Collect.TransactionMemos where

import qualified Penny.Harvest.Locate.Located as Located
import qualified Penny.Tree.Memo.Transaction as Memo.Transaction
import Data.Sequence (Seq, (|>))

-- | Currently in a TopLineMemo

newtype T = T { toSeq :: Seq (Located.T Memo.Transaction.T) }
  deriving (Eq, Ord, Show)

addMemo :: T -> Located.T Memo.Transaction.T -> T
addMemo (T sq) m = T (sq |> m)
