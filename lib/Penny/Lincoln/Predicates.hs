{-# LANGUAGE OverloadedStrings #-}

-- | Functions that return a boolean based upon some criterion that
-- matches something, often a PostFam. Useful when filtering
-- Postings.
module Penny.Lincoln.Predicates
  ( LPdct
  , MakePdct
  , payee
  , number
  , flag
  , postingMemo
  , transactionMemo
  , date
  , qty
  , drCr
  , debit
  , credit
  , commodity
  , account
  , accountLevel
  , accountAny
  , tag
  , reconciled
  ) where


import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as X
import qualified Data.Time as Time
import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.HasText (HasText, text, HasTextList, textList)
import qualified Penny.Lincoln.Queries as Q
import Penny.Lincoln.Ents (ViewedEnt)
import qualified Text.Matchers as M
import qualified Data.Prednote.Pdct as P

type LPdct = P.Pdct ViewedEnt

type MakePdct = M.Matcher -> LPdct

-- * Matching helpers
match
  :: HasText a
  => Text
  -- ^ Description of this field
  -> (ViewedEnt -> a)
  -- ^ Function that returns the field being matched
  -> M.Matcher
  -> LPdct
match t f m = P.operand desc pd
  where
    desc = makeDesc t m
    pd = M.match m . text . f

matchMaybe
  :: HasText a
  => Text
  -- ^ Description of this field
  -> (ViewedEnt -> Maybe a)
  -> M.Matcher
  -> LPdct
matchMaybe t f m = P.operand desc pd
  where
    desc = makeDesc t m
    pd = maybe False (M.match m . text) . f

makeDesc :: Text -> M.Matcher -> Text
makeDesc t m
  = "subject: " <> t
  <> " matcher: " <> M.matchDesc m

-- | Does the given matcher match any of the elements of the Texts in
-- a HasTextList?
matchAny
  :: HasTextList a
  => Text
  -> (ViewedEnt -> a)
  -> M.Matcher
  -> LPdct
matchAny t f m = P.operand desc pd
  where
    desc = makeDesc t m
    pd = any (M.match m) . textList . f

-- | Does the given matcher match the text that is at the given
-- element of a HasTextList? If the HasTextList does not have a
-- sufficent number of elements to perform this test, returns False.
matchLevel
  :: HasTextList a
  => Int
  -> Text
  -> (ViewedEnt -> a)
  -> M.Matcher
  -> LPdct
matchLevel l d f m = P.operand desc pd
  where
    desc = makeDesc ("level " <> X.pack (show l) <> " of " <> d) m
    pd pf = let ts = textList (f pf)
            in if l < 0 || l >= length ts
               then False
               else M.match m (ts !! l)

-- | Does the matcher match the text of the memo? Joins each line of
-- the memo with a space.
matchMemo
  :: Text
  -> (ViewedEnt -> Maybe B.Memo)
  -> M.Matcher
  -> LPdct
matchMemo t f m = P.operand desc pd
  where
    desc = makeDesc t m
    pd = maybe False doMatch . f
    doMatch = M.match m
              . X.intercalate (X.singleton ' ')
              . B.unMemo

matchDelimited
  :: HasTextList a
  => Text
  -- ^ Separator
  -> Text
  -- ^ Label
  -> (ViewedEnt -> a)
  -> M.Matcher
  -> LPdct
matchDelimited sep lbl f m = match lbl f' m
  where
    f' = X.concat . intersperse sep . textList . f

-- * Pattern matching fields

payee :: MakePdct
payee = matchMaybe "payee" Q.payee

number :: MakePdct
number = matchMaybe "number" Q.number

flag :: MakePdct
flag = matchMaybe "flag" Q.flag

postingMemo :: MakePdct
postingMemo = matchMemo "posting memo" Q.postingMemo

transactionMemo :: MakePdct
transactionMemo = matchMemo "transaction memo" Q.transactionMemo

-- * Date

date
  :: Ordering
  -> Time.UTCTime
  -> LPdct
date ord u = P.compareBy (X.pack . show $ u)
             "UTC date and time"
           (\l -> compare (B.toUTC . Q.dateTime $ l) u) ord


qty :: Ordering -> B.Qty -> LPdct
qty o q = P.compareBy (X.pack . show $ q) "quantity"
          (\l -> compare (Q.qty l) q) o


drCr :: B.DrCr -> LPdct
drCr dc = P.operand desc pd
  where
    desc = "entry is a " <> s
    s = case dc of { B.Debit -> "debit"; B.Credit -> "credit" }
    pd pf = Q.drCr pf == dc

debit :: LPdct
debit = drCr B.Debit

credit :: LPdct
credit = drCr B.Credit

commodity :: M.Matcher -> LPdct
commodity = match "commodity" Q.commodity

account :: M.Matcher -> LPdct
account = matchDelimited ":" "account" Q.account

accountLevel :: Int -> M.Matcher -> LPdct
accountLevel i = matchLevel i "account" Q.account

accountAny :: M.Matcher -> LPdct
accountAny = matchAny "any sub-account" Q.account

tag :: M.Matcher -> LPdct
tag = matchAny "any tag" Q.tags

-- | True if a posting is reconciled; that is, its flag is exactly
-- @R@.
reconciled :: LPdct
reconciled = P.operand d p
  where
    d = "posting flag is exactly \"R\" (is reconciled)"
    p = maybe False ((== X.singleton 'R') . B.unFlag) . Q.flag

