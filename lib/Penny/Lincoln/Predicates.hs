-- | Functions that return a boolean based upon some criterion that
-- matches something, often a PostingBox. Useful when filtering
-- Postings.
module Penny.Lincoln.Predicates where

import Data.Text (Text, singleton)

import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.HasText (HasText, text, HasTextList, textList,
                              Delimited(Delimited))
import qualified Penny.Lincoln.Queries as Q
import Penny.Lincoln.Transaction (PostingChild)

-- * Matching helpers

match :: HasText a => (Text -> Bool) -> a -> Bool
match f = f . text

matchMaybe :: HasText a => (Text -> Bool) -> Maybe a -> Bool
matchMaybe f a = case a of
  Nothing -> False
  (Just t) -> match f t

matchAny :: HasTextList a => (Text -> Bool) -> a -> Bool
matchAny f = any f . textList

matchAnyMaybe :: HasTextList a => (Text -> Bool) -> Maybe a -> Bool
matchAnyMaybe f ma = case ma of
  Nothing -> False
  (Just ls) -> matchAny f ls

matchLevel :: HasTextList a => Int -> (Text -> Bool) -> a -> Bool
matchLevel i f a = let ts = textList a in
  if i < 0 || i >= length ts
  then False
  else f (ts !! i)

matchMemo :: (Text -> Bool) -> B.Memo -> Bool
matchMemo f m = f m' where
  m' = text $ Delimited (singleton ' ') (textList m)
  

matchMaybeLevel ::
  HasTextList a
  => Int
  -> (Text -> Bool)
  -> Maybe a
  -> Bool
matchMaybeLevel i f ma = case ma of
  Nothing -> False
  (Just l) -> matchLevel i f l

-- * Pattern matching fields

payee :: (Text -> Bool) -> PostingChild tm pm -> Bool
payee f = matchMaybe f . Q.payee

number :: (Text -> Bool) -> PostingChild tm pm -> Bool
number f = matchMaybe f . Q.number

flag :: (Text -> Bool) -> PostingChild tm pm -> Bool
flag f = matchMaybe f . Q.flag

postingMemo :: (Text -> Bool) -> PostingChild tm pm -> Bool
postingMemo f = matchMemo f . Q.postingMemo

transactionMemo :: (Text -> Bool) -> PostingChild tm pm -> Bool
transactionMemo f = matchMemo f . Q.transactionMemo

-- * Date

date ::
  (B.DateTime -> B.DateTime -> Bool)
  -> B.DateTime
  -> PostingChild tm pm
  -> Bool
date f dt c = f (Q.dateTime c) dt


-- * Qty

qty ::
  (B.Qty -> B.Qty -> Bool)
  -> B.Qty
  -> PostingChild tm pm
  -> Bool
qty f q c = f (Q.qty c) q


-- * DrCr
drCr :: B.DrCr -> PostingChild tm pm -> Bool
drCr dc p = dc == Q.drCr p

debit :: PostingChild tm pm -> Bool
debit p = Q.drCr p == B.Debit

credit :: PostingChild tm pm -> Bool
credit p = Q.drCr p == B.Credit

-- * Matching delimited fields

matchDelimited ::
  HasTextList a
  => Text
  -> (Text -> Bool)
  -> a -> Bool
matchDelimited d f = f . text . Delimited d . textList

-- * Commodity

commodity :: Text -> (Text -> Bool) -> PostingChild tm pm -> Bool
commodity t f = matchDelimited t f
                . B.unCommodity
                . Q.commodity

commodityLevel :: Int -> (Text -> Bool) -> PostingChild tm pm -> Bool
commodityLevel i f = matchLevel i f . Q.commodity

commodityAny :: (Text -> Bool) -> PostingChild tm pm -> Bool
commodityAny f = matchAny f . Q.commodity


-- * Account
account :: Text -> (Text -> Bool) -> PostingChild tm pm -> Bool
account t f = matchDelimited t f
                . B.unAccount
                . Q.account

accountLevel :: Int -> (Text -> Bool) -> PostingChild tm pm -> Bool
accountLevel i f = matchLevel i f . Q.account

accountAny :: (Text -> Bool) -> PostingChild tm pm -> Bool
accountAny f = matchAny f . Q.account

-- * Tags
tag :: (Text -> Bool) -> PostingChild tm pm -> Bool
tag f = matchAny f . Q.tags

