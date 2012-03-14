module Penny.Lincoln.Bits.Memo where

import Penny.Lincoln.TextNonEmpty (TextNonEmpty)

newtype MemoLine = MemoLine { unMemoLine :: TextNonEmpty }
                   deriving (Eq, Ord, Show)

newtype Memo = Memo { unMemo :: [MemoLine] }
             deriving (Eq, Show, Ord)
