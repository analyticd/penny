module Penny.Posting where

import qualified Penny.Bits as B
import qualified Penny.Bits.Qty as Q
import qualified Penny.Bits.Price as Price
import qualified Penny.Bits.Commodity as Commodity
import qualified Penny.Bits.Amount as A
import qualified Penny.Bits.Entry as E
import qualified Penny.Total as T

import qualified Penny.Posting.Unverified.Parent as UParent
import qualified Penny.Posting.Unverified.Posting as UPosting

import Penny.Bits.Qty ( add, mult )
import qualified Penny.Posting.Parent as P
import Penny.Groups.AtLeast2 (
  AtLeast2 ( AtLeast2 ), family )
import Penny.Groups.FamilyMember ( FamilyMember )
import Control.Monad.Exception.Synchronous (
  Exceptional (Exception, Success) , throw )
import qualified Data.Foldable as F
import Data.Monoid ( mconcat )
import Data.Maybe ( catMaybes, fromMaybe )

data Posting =
  Posting { payee :: B.Payee
          , number :: Maybe B.Number
          , cleared :: B.Cleared
          , account :: B.Account
          , entry :: E.Entry
          , price :: Maybe Price.Price
          , tags :: B.Tags
          , memo :: Maybe B.Memo
          , uid :: Maybe B.Uid
          , parent :: P.Parent }

-- | All the Postings in a Transaction:
--
-- * May have different Commodities for the Amount in the Entry.
--
-- * Must have the same Commodity for the Amount in the Value.
--
-- * Must produce a Total whose debits and credits are equal.
newtype Transaction =
  Transaction { unTransaction :: AtLeast2 Posting }
  
data Error = UnbalancedError
           | TooManyInferError
           | CouldNotInferError

-- | Get the Postings from a Transaction, with information on the
-- sibling Postings.
postingFamily :: Transaction -> [FamilyMember Posting]
postingFamily (Transaction ps) = family ps

-- | Makes transactions.
--
-- Step 1. Make sure there is at most 1 posting to infer. If more than
-- one, throw.
--
-- Step 2. Compute value of all non-infer postings and total them.
--
-- Step 3. If there are no postings to infer, total must be
-- balanced. If not, throw.
--
-- Step 4. If there is one posting to infer, total must have a
-- non-naught balance with only one commodity. If not, throw.
--
-- Step 5. Infer the one posting (if needed) and make transaction.
transaction ::
  UParent.Parent
  -> AtLeast2 UPosting.Posting
  -> Exceptional Error Transaction
transaction = undefined

data CostData = CostData E.Entry (Maybe Price.Price) Price.Value
data HasEntry =
  HasEntry CostData
  | NoEntry

hasEntry :: UPosting.Posting -> HasEntry
hasEntry po = case UPosting.cost po of
  UPosting.Blank -> NoEntry
  (UPosting.EntryOnly e) -> let
    v = Price.entryToValue e
    in (HasEntry (CostData e Nothing v))
  (UPosting.EntryPrice e cpu t) -> let
    (p, v) = Price.price t cpu e
    in (HasEntry (CostData e (Just p) v))

{-
toHasEntries :: AtLeast2 UPosting.Posting
                -> AtLeast2 (UPosting.Posting, HasEntry)
toHasEntries = fmap (\p -> (p, hasEntry p))
-}

toCostData :: UPosting.Posting -> Maybe CostData
toCostData po = case UPosting.cost po of
  UPosting.Blank -> Nothing
  (UPosting.EntryOnly e) -> let
    v = Price.entryToValue e
    in Just (CostData e Nothing v)
  (UPosting.EntryPrice e cpu t) -> let
    (p, v) = Price.price t cpu e
    in Just (CostData e (Just p) v)

toHasEntry :: UPosting.Posting -> HasEntry
toHasEntry = maybe NoEntry HasEntry . toCostData

a2toCost :: AtLeast2 UPosting.Posting
            -> Maybe CostData
a2toCost a2 = undefined

{-
toHasEntries ::
  AtLeast2 UPosting.Posting
  -> Either (AtLeast2 (UPosting.Posting, HasEntry))
  (AtLeast2 (UPosting.Posting, CostData))
toHasEntries = let
  g po = case UPosting.cost 

-}
{-
costDataOnly ::
  AtLeast2 (UPosting.Posting, HasEntry)
  -> Either (AtLeast2 (UPosting.Posting, HasEntry))
  (AtLeast2 (UPosting.Posting, CostData))
costDataOnly a2 = let
  p h = case h of (HasEntry _) -> True; _ -> False
  g (po, ha) = 
-}
{-
data NeedsInference = NeedsInference | NoInferenceNeeded

needsInference ::
  AtLeast2 (UPosting.Posting, HasEntry)
  -> Exceptional Error NeedsInference
needsInference a2 = F.foldr f i a2 where
  i = (Success NoInferenceNeeded)
  f (po, he) s = case s of
    (Exception e) -> Exception e
    (Success ni) -> case ni of
      NeedsInference -> case he of
        NoEntry -> Exception TooManyInferError
        (HasEntry _) -> Success NeedsInference
      NoInferenceNeeded -> case he of
        NoEntry -> Success NeedsInference
        (HasEntry _) -> Success NoInferenceNeeded

toNeedsInference ::
  AtLeast2 (UPosting.Posting, HasEntry)
  -> Exceptional Error
  (AtLeast2 (UPosting.Posting, HasEntry), NeedsInference)
toNeedsInference a2 = do
  ni <- needsInference a2
  return (a2, ni)
-}
