-- | These are the bits that are "open"; that is, their constructors
-- are exported. This includes most bits.

module Penny.Lincoln.Bits.Open where

import Data.Text (Text)

newtype SubAccount =
  SubAccount { unSubAccount :: Text }
  deriving (Eq, Ord, Show)

newtype Account = Account { unAccount :: [SubAccount] }
                  deriving (Eq, Show, Ord)

data Amount = Amount { qty :: Qty
                     , commodity :: Commodity }
              deriving (Eq, Show, Ord)

newtype Commodity =
  Commodity { unCommodity :: Text }
  deriving (Eq, Ord, Show)

data DrCr = Debit | Credit deriving (Eq, Show, Ord)

-- | Debit returns Credit; Credit returns Debit
opposite :: DrCr -> DrCr
opposite d = case d of
  Debit -> Credit
  Credit -> Debit

data Entry = Entry { drCr :: DrCr
                   , amount :: Amount }
             deriving (Eq, Show, Ord)

newtype Flag = Flag { unFlag :: Text }
             deriving (Eq, Show, Ord)

newtype Memo = Memo { unMemo :: Text }
             deriving (Eq, Show, Ord)

newtype Number = Number { unNumber :: Text }
                 deriving (Eq, Show, Ord)

newtype Payee = Payee { unPayee :: Text }
              deriving (Eq, Show, Ord)

newtype Tag = Tag { unTag :: Text }
                  deriving (Eq, Show, Ord)

newtype Tags = Tags { unTags :: [Tag] }
               deriving (Eq, Show, Ord)

