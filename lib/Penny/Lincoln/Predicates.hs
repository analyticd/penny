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
  , Comp(..)
  , descComp
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
  , clonedTransactions
  , clonedTopLines
  , clonedPostings
  ) where


import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as X
import qualified Data.Time as Time
import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.HasText (HasText, text, HasTextList, textList)
import qualified Penny.Lincoln.Family.Family as F
import qualified Penny.Lincoln.Queries as Q
import Penny.Lincoln.Transaction (PostFam)
import qualified Penny.Lincoln.Transaction as T
import qualified Text.Matchers as M
import qualified Penny.Steel.Pdct as P

type LPdct = P.Pdct PostFam

type MakePdct = M.Matcher -> LPdct

-- * Matching helpers
match
  :: HasText a
  => Text
  -- ^ Description of this field
  -> (PostFam -> a)
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
  -> (PostFam -> Maybe a)
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
  -> (PostFam -> a)
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
  -> (PostFam -> a)
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
  -> (PostFam -> Maybe B.Memo)
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
  -> (PostFam -> a)
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

-- | Comparisons.
data Comp
  = DLT
  -- ^ Less than

  | DLTEQ
  -- ^ Less than or equal to

  | DEQ
  -- ^ Equal to

  | DGTEQ
  -- ^ Greater than or equal to

  | DGT
  -- ^ Greater than

  | DNE
  -- ^ Not equal to
  deriving Eq

-- | Describes a Comp, and returns a function to actually perform
-- comparisons.
descComp :: Ord a => Comp -> (Text, a -> a -> Bool)
descComp c = case c of
  DLT -> ("less than", (<))
  DLTEQ -> ("less than or equal to", (<=))
  DEQ -> ("equal to", (==))
  DGTEQ -> ("greater than or equal to", (>=))
  DGT -> ("greater than", (>))
  DNE -> ("not equal to", (/=))

date
  :: Comp
  -> Time.UTCTime
  -> LPdct
date c d = P.operand desc pd
  where
    desc = "UTC date is " <> dd <> " " <> X.pack (show d)
    (dd, cmp) = descComp c
    pd pf = (B.toUTC . Q.dateTime $ pf) `cmp` d


qty :: Comp -> B.Qty -> LPdct
qty c q = P.operand desc pd
  where
    desc = "quantity is " <> dd <> " " <> X.pack (show q)
    (dd, cmp) = descComp c
    pd pf = (Q.qty pf) `cmp` q

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

-- | Returns True if these two transactions are clones; that is, if
-- they are identical in all respects except some aspects of their
-- metadata. The metadata that is disregarded when testing for clones
-- pertains to the location of the transaction. (Resembles cloned
-- sheep, which are identical but cannot be in exactly the same
-- place.)
clonedTransactions :: T.Transaction -> T.Transaction -> Bool
clonedTransactions a b =
  let (F.Family ta p1a p2a psa) = T.unTransaction a
      (F.Family tb p1b p2b psb) = T.unTransaction b
  in clonedTopLines ta tb
     && clonedPostings p1a p1b
     && clonedPostings p2a p2b
     && (length psa == length psb)
     && (and (zipWith clonedPostings psa psb))

-- | Returns True if two TopLines are clones. Considers only the
-- non-metadata aspects of the TopLine; the metadata all pertains only
-- to the location of the TopLine. The DateTimes are compared based on
-- both the local time and the time zone; that is, two times that
-- refer to the same instant will not compare as identical if they
-- have different time zones.
clonedTopLines :: T.TopLine -> T.TopLine -> Bool
clonedTopLines t1 t2 =
  (T.tDateTime t1 == T.tDateTime t2)
  && (T.tFlag t1 == T.tFlag t2)
  && (T.tNumber t1 == T.tNumber t2)
  && (T.tPayee t2 == T.tPayee t2)
  && (T.tMemo t1 == T.tMemo t2)

-- | Returns True if two Postings are clones. Considers only the
-- non-location-related aspects of the posting metadata.
clonedPostings :: T.Posting -> T.Posting -> Bool
clonedPostings p1 p2 =
  (T.pPayee p1 == T.pPayee p2)
  && (T.pNumber p1 == T.pNumber p2)
  && (T.pFlag p1 == T.pFlag p2)
  && (T.pAccount p1 == T.pAccount p2)
  && (T.pTags p1 == T.pTags p2)
  && (T.pEntry p1 == T.pEntry p2)
  && (T.pMemo p1 == T.pMemo p2)
  && (T.pInferred p1 == T.pInferred p2)

