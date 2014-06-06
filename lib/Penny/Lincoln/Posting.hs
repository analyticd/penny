module Penny.Lincoln.Posting where

import Data.Text (Text)
import Penny.Lincoln.Common

newtype SubAccount =
  SubAccount { unSubAccount :: Text }
  deriving (Eq, Ord, Show)

newtype Account = Account { unAccount :: [SubAccount] }
                  deriving (Eq, Show, Ord)

data Entry a = Entry
  { amount :: a
  , commodity :: Commodity
  } deriving (Eq, Ord, Show)

-- | Two 'Entry' are equivalent if they have the exact same
-- 'Commodity' (this is case sensitive) and if they have equivalent
-- 'Amount'.  When comparing, the 'Amount' are compared first, and
-- then the 'Commodity'.

instance Equivalent a => Equivalent (Entry a) where
  compareEv (Entry a1 c1) (Entry a2 c2) =
    compareEv a1 a2 <> compare c1 c2

data PostingMeta = PostingMeta
  { topLine :: Line
    -- ^ The line number that the TopLine starts on, excluding the
    -- memo accompanying the TopLine
  , topMemo :: Line
    -- ^ The line number that the memo accompanying the TopLine starts
    -- on
  , pstgLine :: Line
  , filename :: Filename
    -- ^ The file in which the posting appears
  } deriving (Eq, Ord, Show)
