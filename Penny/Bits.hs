module Penny.Bits where

import qualified Penny.TextNonEmpty as NE
import qualified Penny.Bits.Qty as Q
import Data.Time.Clock ( UTCTime )
import Penny.Groups.AtLeast1 ( AtLeast1 )

data DrCr = Debit | Credit deriving Eq

newtype Commodity = Commodity { unCommodity :: NE.TextNonEmpty }
                 deriving (Eq, Ord)

data PriceDesc = UnitPrice | TotalPrice

data Price = Price { priceDesc :: PriceDesc
                   , priceAmt :: Amount }

data Amount = Amount { qty :: Q.Qty
                     , commodity :: Commodity }

newtype Payee = Payee { unPayee :: NE.TextNonEmpty }
                deriving (Eq)

newtype DateTime = DateTime { unDateTime :: UTCTime }

data Cleared = Cleared | NotCleared

newtype Number = Number { unNumber :: NE.TextNonEmpty }
                 deriving (Eq)

newtype SubAccountName =
  SubAccountName { unSubAccountName :: NE.TextNonEmpty }
  deriving (Eq, Ord)

newtype Account = Account { unAccount :: AtLeast1 SubAccountName }

newtype TagName = TagName { unTagName :: NE.TextNonEmpty }
                  deriving Eq

newtype Tags = Tags { unTags :: [TagName] }

newtype Memo =
  Memo { unMemo :: NE.TextNonEmpty }

data Entry = Entry { drCr :: DrCr
                   , amount :: Amount }

