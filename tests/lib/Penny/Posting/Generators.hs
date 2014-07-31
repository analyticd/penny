module Penny.Posting.Generators where

import Penny.Posting
import Test.QuickCheck
import Data.Text.Generators

subAccount :: Gen SubAccount
subAccount = fmap SubAccount $ text arbitrary

account :: Gen Account
account = fmap Account $ listOf subAccount


