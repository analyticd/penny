module Penny.Lincoln.Predicates where

import Data.List (intersperse)
import Data.Text (Text, cons)
import qualified Data.Text as X
import Data.List.NonEmpty (NonEmpty)
import Data.Foldable (toList)
import Data.Maybe (isNothing)

import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.Boxes (PostingBox)
import qualified Penny.Lincoln.Queries as Q
import Penny.Lincoln.TextNonEmpty (TextNonEmpty(TextNonEmpty))

class HasText a where
  text :: a -> Text

instance HasText Text where
  text = id

instance HasText TextNonEmpty where
  text (TextNonEmpty f r) = f `cons` r

instance HasText B.SubAccountName where
  text = text . B.unSubAccountName

instance HasText B.SubCommodity where
  text = text . B.unSubCommodity

instance HasText B.Flag where
  text = text . B.unFlag

instance HasText B.Memo where
  text = text . B.unMemo

instance HasText B.Number where
  text = text . B.unNumber

instance HasText B.Payee where
  text = text . B.unPayee

instance HasText B.Tag where
  text = text . B.unTag
  
data Delimited a = Delimited Text [a]
                 deriving Show

instance HasText a => HasText (Delimited a) where
  text (Delimited sep ts) = X.concat . intersperse sep . map text $ ts

class HasTextList a where
  textList :: a -> [Text]

instance HasText a => HasTextList (NonEmpty a) where
  textList = map text . toList

instance HasTextList B.Account where
  textList = textList . B.unAccount

instance HasTextList B.Commodity where
  textList = textList . B.unCommodity

instance HasTextList B.Tags where
  textList = map text . B.unTags

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

matchMaybeLevel ::
  HasTextList a
  => Int
  -> (Text -> Bool)
  -> Maybe a
  -> Bool
matchMaybeLevel i f ma = case ma of
  Nothing -> False
  (Just l) -> matchLevel i f l

payee :: (Text -> Bool) -> PostingBox t p -> Bool
payee f = matchMaybe f . Q.payee

number :: (Text -> Bool) -> PostingBox t p -> Bool
number f = matchMaybe f . Q.number

flag :: (Text -> Bool) -> PostingBox t p -> Bool
flag f = matchMaybe f . Q.flag

noFlag :: PostingBox t p -> Bool
noFlag = isNothing . Q.flag

postingMemo :: (Text -> Bool) -> PostingBox t p -> Bool
postingMemo f = matchMaybe f . Q.postingMemo

transactionMemo :: (Text -> Bool) -> PostingBox t p -> Bool
transactionMemo f = matchMaybe f . Q.transactionMemo

--
-- Date predicates
--

onOrAfter :: B.DateTime -> PostingBox t p -> Bool
onOrAfter d p = Q.dateTime p >= d

onOrBefore :: B.DateTime -> PostingBox t p -> Bool
onOrBefore d p = Q.dateTime p <= d

before :: B.DateTime -> PostingBox t p -> Bool
before d p = Q.dateTime p < d

after :: B.DateTime -> PostingBox t p -> Bool
after d p = Q.dateTime p > d

dateIs :: B.DateTime -> PostingBox t p -> Bool
dateIs d p = Q.dateTime p == d

--
-- Qty predicates
--

greaterThanOrEqualTo :: B.Qty -> PostingBox t p -> Bool
greaterThanOrEqualTo q p = q >= Q.qty p

lessThanOrEqualTo :: B.Qty -> PostingBox t p -> Bool
lessThanOrEqualTo q p = q <= Q.qty p

greaterThan :: B.Qty -> PostingBox t p -> Bool
greaterThan q p = q > Q.qty p

lessThan :: B.Qty -> PostingBox t p -> Bool
lessThan q p = q < Q.qty p

equals :: B.Qty -> PostingBox t p -> Bool
equals q p = q == Q.qty p

--
-- DrCr predicates
--

drCr :: B.DrCr -> PostingBox t p -> Bool
drCr dc p = dc == Q.drCr p

--
-- Invert
--
invert :: (PostingBox t p -> Bool)
          -> PostingBox t p -> Bool
invert f = not . f

matchDelimited ::
  HasTextList a
  => Text
  -> (Text -> Bool)
  -> a -> Bool
matchDelimited d f = f . text . Delimited d . textList

--
-- Commodity
--
commodity :: Text -> (Text -> Bool) -> PostingBox t p -> Bool
commodity t f = matchDelimited t f
                . B.unCommodity
                . Q.commodity

commodityLevel :: Int -> (Text -> Bool) -> PostingBox t p -> Bool
commodityLevel i f = matchLevel i f . Q.commodity

commodityAny :: (Text -> Bool) -> PostingBox t p -> Bool
commodityAny f = matchAny f . Q.commodity

--
-- Account
--
account :: Text -> (Text -> Bool) -> PostingBox t p -> Bool
account t f = matchDelimited t f
                . B.unAccount
                . Q.account

accountLevel :: Int -> (Text -> Bool) -> PostingBox t p -> Bool
accountLevel i f = matchLevel i f . Q.account

accountAny :: (Text -> Bool) -> PostingBox t p -> Bool
accountAny f = matchAny f . Q.account

--
-- Tags
--
tag :: (Text -> Bool) -> PostingBox t p -> Bool
tag f = matchAny f . Q.tags

--
-- Debit or credit
--
debit :: PostingBox t p -> Bool
debit p = Q.drCr p == B.Debit

credit :: PostingBox t p -> Bool
credit p = Q.drCr p == B.Credit
