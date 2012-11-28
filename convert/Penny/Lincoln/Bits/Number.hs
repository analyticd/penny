module Penny.Lincoln.Bits.Number where

import Penny.Lincoln.TextNonEmpty (TextNonEmpty)

newtype Number = Number { unNumber :: TextNonEmpty }
                 deriving (Eq, Show, Ord)
