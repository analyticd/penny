module Penny.Lincoln.Bits.Account where

import Penny.Lincoln.Groups.TextNonEmpty (TextNonEmpty)
import Penny.Lincoln.Groups.AtLeast1 (AtLeast1)

newtype SubAccountName =
  SubAccountName { unSubAccountName :: TextNonEmpty }
  deriving (Eq, Ord, Show)

newtype Account = Account { unAccount :: AtLeast1 SubAccountName }
                  deriving (Eq, Show, Ord)

