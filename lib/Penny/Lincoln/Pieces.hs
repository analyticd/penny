-- | Things common to Posting and to TopLine.
module Penny.Lincoln.Pieces where

import Data.Text (Text)
import Penny.Lincoln.HasText

-- | There is one item in the list for each line of the memo. Do not
-- include newlines in the texts themselves. However there is nothing
-- to enforce this convention.
newtype Memo = Memo { unMemo :: [Text] }
             deriving (Eq, Show, Ord)

instance HasTextList Memo where
  textList = unMemo

newtype Number = Number { unNumber :: Text }
                 deriving (Eq, Show, Ord)

instance HasText Number where
  text = unNumber

newtype Payee = Payee { unPayee :: Text }
              deriving (Eq, Show, Ord)

instance HasText Payee where
  text = unPayee

newtype Flag = Flag { unFlag :: Text }
             deriving (Eq, Show, Ord)

instance HasText Flag where
  text = unFlag
