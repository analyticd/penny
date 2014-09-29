module Penny.Harvest.Collect.Memo.Posting where

import qualified Penny.Harvest.Locate.Located as Located
import qualified Penny.Tree.Memo.Posting as Memo.Posting
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import qualified Penny.Core.Memo as Memo

newtype T = T { toSeq :: Seq (Located.T Memo.Posting.T) }
  deriving (Eq, Ord, Show)

addMemo :: T -> Located.T Memo.Posting.T -> T
addMemo (T sq) m = T (sq |> m)

toCore :: T -> Memo.T
toCore = Memo.T . fmap (Memo.Posting.toCore . Located.payload)
  . toSeq

empty :: T
empty = T S.empty
