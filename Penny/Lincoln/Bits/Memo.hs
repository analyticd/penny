module Penny.Lincoln.Bits.Memo where

import Penny.Lincoln.Groups.TextNonEmpty (TextNonEmpty)

newtype Memo = Memo { unMemo :: TextNonEmpty }
             deriving (Eq, Show, Ord)
