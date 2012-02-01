module Penny.Lincoln.Bits.Payee where

import Penny.Lincoln.Groups.TextNonEmpty ( TextNonEmpty )

newtype Payee = Payee { unPayee :: TextNonEmpty }
              deriving (Eq, Show, Ord)

