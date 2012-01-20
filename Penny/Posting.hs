{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Penny.Posting where

import Penny.Qty
import Data.Text ( Text )
import Data.Time.Clock ( UTCTime )
import Data.Tree ( Tree )
import qualified Penny.TextNonEmpty as NE

data DrCr = Debit | Credit

newtype Commodity = Commodity { unCommodity :: NE.TextNonEmpty }
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

data Sibling m =
  Sibling { thisSibling :: MetaBox Posting m
          , otherSiblings :: NonEmptyList (MetaBox Posting m) }

data Label m = Label { labelName :: SubAccountName
                     , labelSiblings :: NonEmptyList (Sibling m) }

newtype Trees m = Trees { unTrees :: [Tree (Label m)] }

data MetaBox p m = MetaBox { payload :: p
                           , metadata :: m }
