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
  , filename

  -- * Serials
  , serialPdct
  , MakeSerialPdct
  , fwdGlobalPosting
  , backGlobalPosting
  , fwdFilePosting
  , backFilePosting
  , fwdGlobalTransaction
  , backGlobalTransaction
  , fwdFileTransaction
  , backFileTransaction
  ) where


import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as X
import qualified Data.Time as Time
import qualified Penny.Lincoln.Bits as B
import Penny.Lincoln.HasText (HasText, text, HasTextList, textList)
import qualified Penny.Lincoln.Queries as Q
import Penny.Lincoln.Ents (Posting)
import qualified Penny.Lincoln.Ents as E
import qualified Text.Matchers as M
import qualified Data.Prednote.Pdct as P
import Penny.Lincoln.Serial (forward, backward)

type LPdct = P.Pdct Posting

type MakePdct = M.Matcher -> LPdct

-- * Matching helpers
match
  :: HasText a
  => Text
  -- ^ Description of this field
  -> (Posting -> a)
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
  -> (Posting -> Maybe a)
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
  -> (Posting -> a)
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
  -> (Posting -> a)
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
  -> (Posting -> Maybe B.Memo)
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
  -> (Posting -> a)
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

filename :: M.Matcher -> LPdct
filename = matchMaybe "filename" Q.filename

-- | Makes Pdct based on comparisons against a particular serial.

serialPdct
  :: Text
  -- ^ Name of the serial, e.g. @globalPosting@

  -> (a -> Maybe Int)
  -- ^ How to obtain the serial from the item being examined

  -> Int
  -- ^ The right hand side

  -> Ordering
  -- ^ The Pdct returned will be Just True if the item has a serial
  -- and @compare ser rhs@ returns this Ordering; Just False if the
  -- item has a srerial and @compare@ does not return this Ordering;
  -- Nothing if the item does not have a serial.

  -> P.Pdct a

serialPdct name getSer i o = P.Pdct n (P.Operand f)
  where
    n = "serial " <> name <> " is " <> descCmp <> " "
        <> X.pack (show i)
    descCmp = case o of
      EQ -> "equal to"
      LT -> "less than"
      GT -> "greater than"
    f = fmap (\ser -> compare ser i == o) . getSer

type MakeSerialPdct = Int -> Ordering -> P.Pdct Posting

fwdGlobalPosting :: MakeSerialPdct
fwdGlobalPosting =
  serialPdct "fwdGlobalPosting"
  $ fmap (forward . B.unGlobalPosting)
  . B.pdGlobal
  . E.meta
  . E.headEnt
  . snd

backGlobalPosting :: MakeSerialPdct
backGlobalPosting =
  serialPdct "revGlobalPosting"
  $ fmap (backward . B.unGlobalPosting)
  . B.pdGlobal
  . E.meta
  . E.headEnt
  . snd

fwdFilePosting :: MakeSerialPdct
fwdFilePosting
  = serialPdct "fwdFilePosting"
  $ fmap (forward . B.unFilePosting . B.pFilePosting)
  . B.pdFileMeta
  . E.meta
  . E.headEnt
  . snd

backFilePosting :: MakeSerialPdct
backFilePosting
  = serialPdct "revFilePosting"
  $ fmap (backward . B.unFilePosting . B.pFilePosting)
  . B.pdFileMeta
  . E.meta
  . E.headEnt
  . snd

fwdGlobalTransaction :: MakeSerialPdct
fwdGlobalTransaction
  = serialPdct "fwdGlobalTransaction"
  $ fmap (forward . B.unGlobalTransaction)
  . B.tlGlobal
  . fst

backGlobalTransaction :: MakeSerialPdct
backGlobalTransaction
  = serialPdct "backGlobalTransaction"
  $ fmap (backward . B.unGlobalTransaction)
  . B.tlGlobal
  . fst

fwdFileTransaction :: MakeSerialPdct
fwdFileTransaction
  = serialPdct "fwdFileTransaction"
  $ fmap (forward . B.unFileTransaction . B.tFileTransaction)
  . B.tlFileMeta
  . fst

backFileTransaction :: MakeSerialPdct
backFileTransaction
  = serialPdct "backFileTransaction"
  $ fmap (backward . B.unFileTransaction . B.tFileTransaction)
  . B.tlFileMeta
  . fst
