-- | Functions that return a boolean based upon some criterion that
-- matches something, often a PostingBox. Useful when filtering
-- Postings.
module Penny.Lincoln.Predicates where

import Data.Text (Text, singleton)

import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.HasText (HasText, text, HasTextList, textList,
                              Delimited(Delimited))
import qualified Penny.Lincoln.Queries as Q
import Penny.Lincoln.Transaction (PostFam)

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

payee :: (Text -> Bool) -> PostFam -> Bool
payee f = matchMaybe f . Q.payee

number :: (Text -> Bool) -> PostFam -> Bool
number f = matchMaybe f . Q.number

flag :: (Text -> Bool) -> PostFam -> Bool
flag f = matchMaybe f . Q.flag

postingMemo :: (Text -> Bool) -> PostFam -> Bool
postingMemo f = matchMemo f . Q.postingMemo

transactionMemo :: (Text -> Bool) -> PostFam -> Bool
transactionMemo f = matchMemo f . Q.transactionMemo

-- * Date

date ::
  (B.DateTime -> Bool)
  -> PostFam
  -> Bool
date f c = f (Q.dateTime c)


-- * Qty

qty ::
  (B.Qty -> Bool)
  -> PostFam
  -> Bool
qty f c = f (Q.qty c)


-- * DrCr
drCr :: B.DrCr -> PostFam -> Bool
drCr dc p = dc == Q.drCr p

debit :: PostFam -> Bool
debit p = Q.drCr p == B.Debit

credit :: PostFam -> Bool
credit p = Q.drCr p == B.Credit

-- * Matching delimited fields

matchDelimited ::
  HasTextList a
  => Text
  -> (Text -> Bool)
  -> a -> Bool
matchDelimited d f = f . text . Delimited d . textList

-- * Commodity

commodity :: Text -> (Text -> Bool) -> PostFam -> Bool
commodity t f = matchDelimited t f
                . B.unCommodity
                . Q.commodity

commodityLevel :: Int -> (Text -> Bool) -> PostFam -> Bool
commodityLevel i f = matchLevel i f . Q.commodity

commodityAny :: (Text -> Bool) -> PostFam -> Bool
commodityAny f = matchAny f . Q.commodity


-- * Account
account :: Text -> (Text -> Bool) -> PostFam -> Bool
account t f = matchDelimited t f
                . B.unAccount
                . Q.account

accountLevel :: Int -> (Text -> Bool) -> PostFam -> Bool
accountLevel i f = matchLevel i f . Q.account

accountAny :: (Text -> Bool) -> PostFam -> Bool
accountAny f = matchAny f . Q.account

-- * Tags
tag :: (Text -> Bool) -> PostFam -> Bool
tag f = matchAny f . Q.tags

-- * Combining predicates
(&&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(&&&) l r = \a -> l a && r a

infixr 3 &&&

(|||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(|||) l r = \a -> l a || r a

infixr 2 |||
