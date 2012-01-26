module Penny.Bits where

import qualified Penny.TextNonEmpty as NE
import qualified Penny.Bits.Qty as Q
import qualified Penny.Bits.Amount as A

import Data.Time.Clock ( UTCTime )
import Penny.Groups.AtLeast1 ( AtLeast1 )


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


