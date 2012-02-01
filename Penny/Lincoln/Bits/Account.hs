module Penny.Lincoln.Bits.Account where

import Penny.Lincoln.TextNonEmpty (TextNonEmpty)
import Data.List.NonEmpty (NonEmpty)

newtype SubAccountName =
  SubAccountName { unSubAccountName :: TextNonEmpty }
  deriving (Eq, Ord, Show)

newtype Account = Account { unAccount :: NonEmpty SubAccountName }
                  deriving (Eq, Show, Ord)

