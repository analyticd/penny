module Penny.Harvest.Collected.Error.Inline where

-- | Errors that occur while collecting lines.
data T
  = PostingWithoutTopLine
  | TransactionMemoWithoutTopLine
  | PostingMemoWithoutPosting
  deriving (Eq, Ord, Show)

