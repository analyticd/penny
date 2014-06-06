-- | Things common to Posting and to TopLine.
module Penny.Lincoln.Pieces where

import Data.Text (Text)

-- | There is one item in the list for each line of the memo. Do not
-- include newlines in the texts themselves. However there is nothing
-- to enforce this convention.
newtype Memo = Memo { unMemo :: [Text] }
             deriving (Eq, Show, Ord)

newtype Number = Number { unNumber :: Text }
                 deriving (Eq, Show, Ord)

newtype Payee = Payee { unPayee :: Text }
              deriving (Eq, Show, Ord)

newtype Flag = Flag { unFlag :: Text }
             deriving (Eq, Show, Ord)

