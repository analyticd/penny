module Penny.Posting.Shrinkers where

import Data.Text.Shrinkers
import Penny.Posting
import Test.QuickCheck

subAccount :: SubAccount -> [SubAccount]
subAccount = fmap SubAccount . text shrink . unSubAccount

account :: Account -> [Account]
account = fmap Account . shrinkList subAccount . unAccount

