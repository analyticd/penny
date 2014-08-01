module Penny.Posting.Shrinkers where

import Data.Text.Shrinkers
import Penny.Serial.Shrinkers
import Penny.Posting
import Test.QuickCheck
import Prelude.Shrinkers
import Penny.Common.Shrinkers
import Prelude hiding (maybe)

subAccount :: SubAccount -> [SubAccount]
subAccount = fmap SubAccount . text shrink . unSubAccount

account :: Account -> [Account]
account = fmap Account . shrinkList subAccount . unAccount

postingMeta :: PostingMeta -> [PostingMeta]
postingMeta (PostingMeta l gs fs) =
  [ PostingMeta l' gs' fs' | (l', gs', fs') <-
    tuple3 line serial serial (l, gs, fs) ]

tag :: Tag -> [Tag]
tag = fmap Tag . text shrink . unTag

tags :: Tags -> [Tags]
tags = fmap Tags . shrinkList tag . unTags

postingData :: PostingData -> [PostingData]
postingData (PostingData m n f p t a) =
  [ PostingData m' n' f' p' t' a' | (m', n', f', p', (t', a')) <-
    tuple5 memo number flag payee (tuple2 tags account)
           (m, n, f, p, (t, a)) ]

posting :: Posting -> [Posting]
posting (Posting d m) =
  [ Posting d' m' | (d', m') <-
    tuple2 postingData (maybe postingMeta) (d, m) ]
