{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Penny.Posting where

import Penny.Qty
import Data.Text ( Text )
import Data.Time.Clock ( UTCTime )
import qualified Penny.TextNonEmpty as NE
import Penny.Groups.FamilyMember
import Penny.Groups.AtLeast1

data DrCr = Debit | Credit deriving Eq

newtype Commodity = Commodity { unCommodity :: NE.TextNonEmpty }
                 deriving (Eq, Ord)

data PriceDesc = UnitPrice | TotalPrice

data Price = Price { priceDesc :: PriceDesc
                   , priceAmt :: Amount }

data Amount = Amount { qty :: Qty
                     , commodity :: Commodity }

newtype Payee = Payee { unPayee :: NE.TextNonEmpty }
                deriving (Eq)

newtype DateTime = DateTime { unDateTime :: UTCTime }

data Cleared = Cleared | NotCleared

newtype Number = Number { unNumber :: Text }
                 deriving (Eq)

newtype SubAccountName = SubAccountName { unSubAccountName :: NE.TextNonEmpty }
                    deriving (Eq, Ord)

newtype Account = Account { unAccount :: AtLeast1 SubAccountName }

newtype TagName = TagName { unTagName :: NE.TextNonEmpty }
                  deriving Eq

newtype Tags = Tags { unTags :: [TagName] }

newtype Comment = Comment { unComment :: Text }

data Entry = Entry { drCr :: DrCr
                   , amount :: Amount }

data Posting =
  Posting { payee :: Payee
          , dateTime :: DateTime
          , cleared :: Cleared
          , number :: Maybe Number
          , account :: Account
          , entry :: Entry
          , price :: Maybe Price
          , tags :: Tags
          , comment :: Maybe Comment }

