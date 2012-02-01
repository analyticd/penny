module Penny.Lincoln.Bits.Flag where

import Penny.Lincoln.TextNonEmpty (TextNonEmpty)

newtype Flag = Flag { unFlag :: TextNonEmpty }
             deriving (Eq, Show, Ord)

