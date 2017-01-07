{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

-- | Checks the balances of accounts.  Useful to keep your ledgers
-- consistent with the statements that come from the bank.

module Penny.BalanceCheck (checkBalances) where

import Control.Monad (join)
import Control.Lens (view, unsnoc, to, _2, _Wrapped', (&))
import Data.Monoid ((<>))
import Data.Time (Day)
import Data.Foldable (toList)
import qualified Data.Foldable as Fdbl
import qualified Data.Map as M
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text, unpack, pack)
import Rainbow (Chunk)
import qualified Rainbow

import Penny.Account
import Penny.Amount (Amount(Amount))
import Penny.Balance
import Penny.Commodity
import Penny.Copper.Copperize
import Penny.Decimal
import Penny.Ents
import Penny.Polar
import Penny.Positive
import Penny.Tranche
import Penny.Transaction
import Penny.Troika

{-

-- | Given map has no more balances.
noMoreBalances :: Balance -> TestTree
noMoreBalances (Balance m) =
  testCase "no commodities not specified are present"
  . assertBool "commodities not specified are present"
  . M.null
  . M.filter ((/= 0) . _coefficient)
  $ m

-- | Checks the map for the given balance.  Returns a new map with the
-- balance removed, if it was there.

lookForBalanceItem
  :: (Commodity, Pole, DecPositive)
  -> ([TestTree], Balance)
  -> ([TestTree], Balance)
lookForBalanceItem (cy, tgtPole, tgtPos) (rest, Balance balMap)
  = (tt : rest, Balance balMap')
  where
    (maybeBal, balMap') = M.updateLookupWithKey (\_ _ -> Nothing) cy balMap
    desc = "a " <> side <> " balance of " <> amt
        where
          side | tgtPole == debit = "debit"
               | otherwise = "credit"
          amt = decimalText (Right Nothing) . fmap c'Integer'Positive $ tgtPos
    tt = testCase (unpack ("commodity " <> cy <> " has " <> desc)) test
    test = case maybeBal of
      Nothing -> assertFailure "commodity has no balance"
      Just bal -> case stripDecimalSign bal of
        Left _ -> assertFailure "balance is zero"
        Right (balDecPos, balPole)
          | balPole /= tgtPole -> assertFailure "balance is on wrong side"
          | cmpPositive (/=) balDecPos tgtPos ->
              assertFailure "balance is on correct side but has wrong magnitude"
          | otherwise -> return ()

-- | Checks a single balance.
checkBalance
  :: Balance
  -> Seq (Commodity, Pole, DecPositive)
  -> [TestTree]
checkBalance bal = checkBal . foldr lookForBalanceItem ([], bal) . toList
  where
    checkBal (tts, bal) = noMoreBalances bal : tts

checkDay
  :: Seq (TransactionX a)
  -> Seq Text
  -> (Day, Seq (Commodity, Pole, DecPositive))
  -> TestTree
checkDay txns na (dy, sq) = testGroup desc tts
  where
    desc = "has these reconciled balances "
         <> "at the end of " <> show dy
    tts = checkBalance bal sq
    bal = maybe mempty (view (_2 . balance)) $ unsnoc clatches
    clatches = clatchesFromTransactions mempty pdConv mempty (const True) txns
    pdConv conv = reconciled conv
      && view (account . to (== na)) conv
      && view (day . to pdDate) conv
    pdDate = (<= dy)

checkBalances
  :: Seq (TransactionX a)
  -- ^ All transactions

  -> Seq ( Seq Text
         , Seq (Day, Seq (Commodity, Pole, DecPositive)))
  -- ^ A sequence.  First in the pair is the account name.  Second in
  -- the pair is another sequence of tuples @(a, b, c, d)@, where @a@
  -- is the year, @b@ is the month, @c@ is the day, and @d@ is another
  -- sequence of tuples @(e, f, g)@, where @e@ is the commodity of the
  -- balance, @f@ is the coefficient, and @g@ is the exponent.
  -> TestTree
checkBalances txns = testGroup desc . toList . fmap (checkAccount txns)
  where
    desc = "check account balances"

checkAccount
  :: Seq (TransactionX a)
  -- ^ All transactions
  -> (Seq Text, Seq (Day, Seq (Commodity, Pole, DecPositive)))
  -> TestTree
checkAccount txns (acct, sq)
  = testGroup desc . toList . fmap (checkDay txns acct) $ sq
  where
    desc = "account " <> show (toList $ fmap unpack acct)
-}

-- | Checks the balance of a particular account.
checkAccount
  :: Seq (Transaction a)
  -> (Account, Seq (Day, Seq (Commodity, Pole, DecPositive)))
  -> (Seq (Chunk Text), Bool)
checkAccount txns (tgtAcct, days) = (reports, cumulative)
  where
    reports = thisAcct <> join (fmap fst results)
    thisAcct =
      [ resultChunk cumulative
      , Rainbow.chunk $ " in account " <> pack (show tgtAcct) <> "\n"
      ]
    cumulative = all snd results
    daysAndAmts = join . fmap (filterAccount tgtAcct) $ txns
    results = fmap (checkDay daysAndAmts) days

-- | Checks the balance of a particular day, along with its
-- commodities, poles, and DecPositive.
checkDay
  :: Seq (Day, Amount)
  -- ^ Postings in this account
  -> (Day, Seq (Commodity, Pole, DecPositive))
  -- ^ Check this day
  -> (Seq (Chunk Text), Bool)
  -- ^ Report, and result.
checkDay pstgs (tgtDay, tgtBals) = (reports, cumulative)
  where
    reports = thisDay <> join (fmap fst results)
      where
        thisDay =
          [ Rainbow.chunk "  "
          , resultChunk cumulative
          , Rainbow.chunk (" on day " <> show tgtDay)
          , Rainbow.chunk "\n"
          ]
    cumulative = all snd results
    bal = getBalanceOnDay tgtDay pstgs
    results = fmap (checkBalance bal) tgtBals

-- | Checks the balance of a particular commodity, pole, and DecPositive.
checkBalance
  :: Balance
  -> (Commodity, Pole, DecPositive)
  -> (Seq (Chunk Text), Bool)
  -- ^ A descriptive report.  Also, True if the balance is OK, False if not.
checkBalance bal (cy, tgtPole, tgtBal) = (desc, result)
  where
    desc =
      [ Rainbow.chunk "    "
      , resultChunk result
      , Rainbow.chunk (" commodity: " <> cy)
      , Rainbow.chunk (" target side: " <> getSideTxt (Just tgtPole))
      , Rainbow.chunk (" target balance: " <> tgtBalTxt)
      , Rainbow.chunk
          (" actual side: " <> getSideTxt (fmap snd mayActualBalAndSide))
      , Rainbow.chunk (" actual balance: " <> actualBalanceTxt)
      , Rainbow.chunk ("\n")
      ]
    result = case mayActualBalAndSide of
      Nothing -> False
      Just (actBal, actSide) -> cmpPositive (==) actBal tgtBal && actSide == tgtPole
    getSideTxt maySd = case maySd of
      Nothing -> "(none)"
      Just sd
        | sd == debit -> "debit"
        | otherwise -> "credit"
    tgtBalTxt = decimalText (Right Nothing) . fmap c'Integer'Positive $ tgtBal
    actualBalanceTxt = case mayActualBalAndSide of
      Nothing -> "(none)"
      Just (dec, _) -> decimalText (Right Nothing) . fmap c'Integer'Positive $ dec
    mayActualBalAndSide = do
      balDec <- M.lookup cy . view _Wrapped' $ bal
      either (const Nothing) Just . stripDecimalSign $ balDec

resultChunk :: Bool -> Chunk Text
resultChunk b
  | b = Rainbow.chunk "[ OK ]" & Rainbow.fore Rainbow.green
  | otherwise = Rainbow.chunk "[FAIL]" & Rainbow.fore Rainbow.red

-- | Gets the balance for a set of Transaction.
getBalanceOnDay
  :: Day
  -- ^ Only get postings that are on or before this Day.
  -> Seq (Day, Amount)
  -> Balance
getBalanceOnDay dy
  = Fdbl.foldl' f mempty
  . fmap snd
  . Seq.filter ((<= dy) . fst)
  where
    f bal amt = bal <> (c'Balance'Amount amt)


-- | Takes a list of Transaction and pulls only the reconciled
-- postings that are from a single account.
filterAccount
  :: Account
  -> Transaction a
  -> Seq (Day, Amount)
filterAccount acct txn
  = fmap extract . Seq.filter pd . balancedToSeqEnt . _postings $ txn
  where
    extract (troika, _) = (view (topLine . day) txn, Amount cy dec)
      where
        cy = view commodity troika
        dec = c'Decimal'Troika troika
    pd (_, postline)
        = view account postline == acct
        && reconciled postline

checkBalances
  :: Seq (Transaction a)
  -- ^ All transactions
  -> Seq (Account, Seq (Day, Seq (Commodity, Pole, DecPositive)))
  -> (Seq (Chunk Text), Bool)
  -- ^ Returns a report showing whether each account passed or failed.
  -- Also returns True if all accounts were OK, or False if there were
  -- any failures.
checkBalances txns acctsAndDays = (reports, cumulative)
  where
    results = fmap (checkAccount txns) acctsAndDays
    reports = join . fmap fst $ results
    cumulative = all snd results
