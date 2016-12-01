module Penny.Account where

import Data.Text (Text)
import Data.Sequence (Seq)

type SubAccount = Text

-- | An account contains a set of related postings.  It is a sequence
-- of 'SubAccount', with each successive 'SubAccount' being a subset
-- of the last, such as
--
-- @["Assets", "Current", "Checking"]@
type Account = Seq SubAccount
