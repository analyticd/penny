{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Penny.Posting where

import Data.Decimal ( Decimal )
import Data.Text ( Text )
import Data.Time.Clock ( UTCTime )
import Data.Tree ( Tree )

data DrCr = Debit | Credit

newtype Qty = Qty { unQty :: Decimal }
              deriving (Eq)

newtype Commodity = Commodity { unCommodity :: Text }
                 deriving (Eq)

data PriceDesc = UnitPrice | TotalPrice

data Price = Price { priceDesc :: PriceDesc
                   , priceAmt :: Amount }

data Amount = Amount { qty :: Qty
                     , commodity :: Commodity }

newtype Payee = Payee { unPayee :: Text }
                deriving (Eq)

newtype DateTime = DateTime { unDateTime :: UTCTime }

data Cleared = Cleared | NotCleared

newtype Number = Number { unNumber :: Text }
                 deriving (Eq)

newtype SubAccountName = SubAccountName { unSubAccountName :: Text }
                    deriving Eq

newtype Account = Account { unAccount :: [SubAccountName] }

newtype TagName = TagName { unTagName :: Text }
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

data NonEmptyList a = NonEmptyList { first :: a
                                   , rest :: [a] }

data Sibling = Sibling { thisSibling :: Posting
                       , otherSiblings :: NonEmptyList Posting }

data Label = Label { labelName :: SubAccountName
                   , labelSiblings :: NonEmptyList Sibling }

newtype Trees = Trees { unTrees :: [Tree Label] }


