module Penny.Lincoln.Bits.Payee where

import Penny.Lincoln.TextNonEmpty ( TextNonEmpty )

newtype Payee = Payee { unPayee :: TextNonEmpty }
              deriving (Eq, Show, Ord)

