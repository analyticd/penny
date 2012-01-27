module Penny.Bits where

import qualified Penny.TextNonEmpty as NE

import Data.Time.Clock ( UTCTime )
import Penny.Groups.AtLeast1 ( AtLeast1 )


newtype Payee = Payee { unPayee :: NE.TextNonEmpty }
                deriving (Eq)

newtype DateTime = DateTime { unDateTime :: UTCTime }

newtype Flag = Flag { unFlag :: Char }

newtype Number = Number { unNumber :: NE.TextNonEmpty }
                 deriving (Eq)

newtype Uid = Uid { unUid :: NE.TextNonEmpty }
              deriving Eq

newtype SubAccountName =
  SubAccountName { unSubAccountName :: NE.TextNonEmpty }
  deriving (Eq, Ord)

newtype Account = Account { unAccount :: AtLeast1 SubAccountName }

newtype TagName = TagName { unTagName :: NE.TextNonEmpty }
                  deriving Eq

newtype Tags = Tags { unTags :: [TagName] }

newtype Memo =
  Memo { unMemo :: NE.TextNonEmpty }


