module Penny.Lincoln.Predicates where

import Data.Text (Text, singleton)

import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.Boxes (PostingBox)
import Penny.Lincoln.HasText (HasText, text, HasTextList, textList,
                              Delimited(Delimited))
import qualified Penny.Lincoln.Queries as Q

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

payee :: (Text -> Bool) -> PostingBox -> Bool
payee f = matchMaybe f . Q.payee

number :: (Text -> Bool) -> PostingBox -> Bool
number f = matchMaybe f . Q.number

flag :: (Text -> Bool) -> PostingBox -> Bool
flag f = matchMaybe f . Q.flag

postingMemo :: (Text -> Bool) -> PostingBox -> Bool
postingMemo f = matchMemo f . Q.postingMemo

transactionMemo :: (Text -> Bool) -> PostingBox -> Bool
transactionMemo f = matchMemo f . Q.transactionMemo

-- * Flexible comparisons

-- | A versatile way to compare things.
data Comparer = LessThan
                | LessThanEQ
                | Equals
                | GreaterThan
                | GreaterThanEQ
                | NotEquals
                deriving Show

-- | Returns a function that compares an item against something and
-- returns True if the item is within the range specified, or False if
-- not.
comp ::
  Ord b
  => Comparer 
  -- ^ The comparison must return this to be successful
  
  -> b
  -- ^ Right hand side of the comparison
  
  -> (a -> b)
  -- ^ Function to convert an item to the left hand side of the
  -- comparison
  
  -> a
  -- ^ Left hand side of the comparison (before being converted by the
  -- function above)
  
  -> Bool
comp c b f a = let r = compare (f a) b in
  case c of
    LessThan -> r == LT
    LessThanEQ -> r == LT || r == EQ
    Equals -> r == EQ
    GreaterThan -> r == GT
    GreaterThanEQ -> r == GT || r == EQ
    NotEquals -> r /= EQ

-- * Date

date :: Comparer -> B.DateTime -> PostingBox -> Bool
date c d = comp c d Q.dateTime

-- * Qty

qty :: Comparer -> B.Qty -> PostingBox -> Bool
qty c q = comp c q Q.qty

-- * DrCr
drCr :: B.DrCr -> PostingBox -> Bool
drCr dc p = dc == Q.drCr p

debit :: PostingBox -> Bool
debit p = Q.drCr p == B.Debit

credit :: PostingBox -> Bool
credit p = Q.drCr p == B.Credit

-- * Matching delimited fields

matchDelimited ::
  HasTextList a
  => Text
  -> (Text -> Bool)
  -> a -> Bool
matchDelimited d f = f . text . Delimited d . textList

-- * Commodity

commodity :: Text -> (Text -> Bool) -> PostingBox -> Bool
commodity t f = matchDelimited t f
                . B.unCommodity
                . Q.commodity

commodityLevel :: Int -> (Text -> Bool) -> PostingBox -> Bool
commodityLevel i f = matchLevel i f . Q.commodity

commodityAny :: (Text -> Bool) -> PostingBox -> Bool
commodityAny f = matchAny f . Q.commodity


-- * Account
account :: Text -> (Text -> Bool) -> PostingBox -> Bool
account t f = matchDelimited t f
                . B.unAccount
                . Q.account

accountLevel :: Int -> (Text -> Bool) -> PostingBox -> Bool
accountLevel i f = matchLevel i f . Q.account

accountAny :: (Text -> Bool) -> PostingBox -> Bool
accountAny f = matchAny f . Q.account

-- * Tags
tag :: (Text -> Bool) -> PostingBox -> Bool
tag f = matchAny f . Q.tags

