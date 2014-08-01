module Penny.Posting.Coarbitrary where

import Penny.Posting
import Test.QuickCheck
import Penny.Common.Coarbitrary
import Data.Text.Coarbitrary
import Prelude.Coarbitrary
import Prelude hiding (maybe)
import Penny.Serial.Coarbitrary

subAccount :: SubAccount -> Gen b -> Gen b
subAccount (SubAccount t) = text t

account :: Account -> Gen b -> Gen b
account (Account x) = list subAccount x

postingMeta :: PostingMeta -> Gen b -> Gen b
postingMeta (PostingMeta l s1 s2) =
  line l . serial s1 . serial s2

tag :: Tag -> Gen b -> Gen b
tag (Tag x) = text x

tags :: Tags -> Gen b -> Gen b
tags (Tags x) = list tag x

postingData :: PostingData -> Gen b -> Gen b
postingData (PostingData m n f p t a) =
  memo m . number n . flag f . payee p . tags t . account a

posting :: Posting -> Gen b -> Gen b
posting (Posting d m) = postingData d . maybe postingMeta m
