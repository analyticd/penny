module Penny.Account where

import Data.Text (Text)
import Data.Sequence (Seq)

type SubAccount = Text

type Account = Seq SubAccount
