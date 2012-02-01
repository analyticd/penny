module Penny.Lincoln.Bits.Memo where

import Penny.Lincoln.TextNonEmpty (TextNonEmpty)

newtype Memo = Memo { unMemo :: TextNonEmpty }
             deriving (Eq, Show, Ord)
