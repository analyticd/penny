{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

-- | Checks the balances of accounts.  Useful to keep your ledgers
-- consistent with the statements that come from the bank.

module Penny.BalanceCheck (checkBalances) where

import Control.Monad (join)
import Control.Lens (view, _Wrapped', (&))
import qualified Control.Lens as Lens
import Data.Monoid ((<>))
import Data.Time (Day)
import qualified Data.Foldable as Fdbl
import qualified Data.Map as M
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Sequence.NonEmpty (nonEmptySeqToSeq)
import Data.Text (Text, pack)
import Rainbow (Chunk)
import qualified Rainbow
import Turtle.Bytes (procs)
import Turtle.Shell (liftIO)
import qualified Turtle.Shell as Shell

import Penny.Account
import Penny.Amount (Amount(Amount))
import Penny.Balance
import Penny.Commodity
import Penny.Copper (parseConvertProofIO)
import Penny.Copper.Copperize
import Penny.Copper.Tracompri
import Penny.Decimal
import Penny.Ents
import Penny.Polar
import Penny.Positive
import Penny.SeqUtil (catMaybes)
import Penny.Tranche
import Penny.Transaction
import Penny.Troika
import Penny.Unix

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
          , Rainbow.chunk (" on day " <> pack (show tgtDay))
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

pureCheckBalances
  :: Seq (Transaction a)
  -- ^ All transactions
  -> Seq (Account, Seq (Day, Seq (Commodity, Pole, DecPositive)))
  -> (Seq (Chunk Text), Bool)
  -- ^ Returns a report showing whether each account passed or failed.
  -- Also returns True if all accounts were OK, or False if there were
  -- any failures.
pureCheckBalances txns acctsAndDays = (reports, cumulative)
  where
    results = fmap (checkAccount txns) acctsAndDays
    reports = join . fmap fst $ results
    cumulative = all snd results

-- | Checks balances and gives a visual report.
loadAndCheckBalances
  :: Seq (Account, Seq (Day, Seq (Commodity, Pole, DecPositive)))
  -- ^ Accounts and balances to check
  -> Seq Text
  -- ^ List of filenames to load
  -> IO (Seq (Chunk Text), Bool)
loadAndCheckBalances toCheck loads = do
  neSeqs <- parseConvertProofIO loads
  let txns
        = catMaybes
        . fmap (Lens.preview _Tracompri'Transaction)
        . join
        . nonEmptySeqToSeq
        $ neSeqs
  return (pureCheckBalances txns toCheck)

-- | Checks balances.  Sends output to @less@ with 256 colors.
checkBalances
  :: Seq (Account, Seq (Day, Seq (Commodity, Pole, DecPositive)))
  -- ^ Accounts and balances to check
  -> Seq Text
  -- ^ List of filenames to load
  -> IO ()
checkBalances toCheck files = do
  (results, _) <- liftIO $ loadAndCheckBalances toCheck files
  maker <- liftIO Rainbow.byteStringMakerFromEnvironment
  let strings = Rainbow.chunksToByteStrings maker (Fdbl.toList results)
  procs "less" lessOpts (Shell.select strings)
