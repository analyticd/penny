module Penny.Posting.Generators where

import Control.Applicative
import Penny.Posting
import Test.QuickCheck
import Data.Text.Generators
import Penny.Common.Generators

subAccount :: Gen SubAccount
subAccount = fmap SubAccount $ text arbitrary

account :: Gen Account
account = fmap Account $ listOf subAccount

tag :: Gen Tag
tag = fmap Tag $ text arbitrary

tags :: Gen Tags
tags = fmap Tags $ listOf tag

postingData :: Gen PostingData
postingData =
  PostingData
  <$> memo
  <*> number
  <*> flag
  <*> payee
  <*> tags
  <*> account

-- | Generates a 'Posting' with no metadata.
posting :: Gen Posting
posting = Posting <$> postingData <*> pure Nothing
