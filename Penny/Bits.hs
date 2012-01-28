module Penny.Bits where

import qualified Penny.TextNonEmpty as NE

import Data.Time ( UTCTime )
import Penny.Groups.AtLeast1 ( AtLeast1 )


newtype Payee = Payee { unPayee :: NE.TextNonEmpty }
                deriving (Eq, Show, Ord)

newtype DateTime = DateTime { unDateTime :: UTCTime }
                   deriving (Eq, Show, Ord)

newtype Flag = Flag { unFlag :: Char }
               deriving (Eq, Show, Ord)

newtype Number = Number { unNumber :: NE.TextNonEmpty }
                 deriving (Eq, Show, Ord)

newtype SubAccountName =
  SubAccountName { unSubAccountName :: NE.TextNonEmpty }
  deriving (Eq, Ord, Show)

newtype Account = Account { unAccount :: AtLeast1 SubAccountName }
                  deriving (Eq, Show, Ord)

newtype TagName = TagName { unTagName :: NE.TextNonEmpty }
                  deriving (Eq, Show, Ord)

newtype Tags = Tags { unTags :: [TagName] }
               deriving (Eq, Show, Ord)

newtype Memo =
  Memo { unMemo :: NE.TextNonEmpty }
  deriving (Eq, Show, Ord)


