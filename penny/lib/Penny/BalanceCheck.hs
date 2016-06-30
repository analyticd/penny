{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Penny.BalanceCheck (checkBalances) where

import Control.Lens (view, unsnoc, to, _2)
import Data.Monoid ((<>))
import Data.Time (fromGregorian)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Sequence (Seq)
import Data.Text (Text, unpack)
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit ((@=?), assertBool, testCase, assertFailure)

import Penny.Balance
import Penny.Clatch
import Penny.Commodity
import Penny.Decimal
import Penny.NonNegative

-- | Given map has no more balances.
noMoreBalances :: Balance -> TestTree
noMoreBalances (Balance m) =
  testCase "no commodities not specified are present"
  $ assertBool "commodities not specified are present" (M.null m)

-- | Checks the map for the given balance.  Returns a new map with the
-- balance removed, if it was there.

lookForBalanceItem
  :: (Commodity, Integer, Integer)
  -- ^ Desired balance item
  -> ([TestTree], Balance)
  -> ([TestTree], Balance)
lookForBalanceItem (cy, sig, exptInt) (rest, Balance balMap)
  = (tt : rest, Balance balMap')
  where
    (maybeBal, balMap') = M.updateLookupWithKey (\_ _ -> Nothing) cy balMap
    tt = testCase ("commodity " <> unpack cy <>
      " has a balance with coefficient " <> show sig <> " and exponent "
      <> show exptInt) . maybe (assertFailure "balance does not exist") id $ do
        bal <- maybeBal
        expt <- c'NonNegative'Integer exptInt
        return $ Exponential sig expt @=? bal

-- | Checks a single balance.
checkBalance
  :: Balance
  -> Seq (Commodity, Integer, Integer)
  -> [TestTree]
checkBalance bal = checkBal . foldr lookForBalanceItem ([], bal) . toList
  where
    checkBal (tts, bal) = noMoreBalances bal : tts

checkDay
  :: Seq (Transaction a)
  -> Seq Text
  -> (Integer, Int, Int, Seq (Commodity, Integer, Integer))
  -> TestTree
checkDay txns na (yr, mo, da, sq) = testGroup desc tts
  where
    desc = "has these reconciled balances "
         <> "at the end of " <> show yr <> "-" <> show mo <> "-" <> show da
    tts = checkBalance bal sq
    bal = maybe mempty (view (_2 . balance)) $ unsnoc clatches
    clatches = clatchesFromTransactions mempty pdConv mempty (const True) txns
    pdConv conv = reconciled conv
      && view (account . to (== na)) conv
      && view (day . to pdDate) conv
    pdDate = (<= fromGregorian yr mo da)

checkBalances
  :: Seq (Transaction a)
  -- ^ All transactions

  -> Seq ( Seq Text
         , Seq (Integer, Int, Int, Seq (Commodity, Integer, Integer)))
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
  :: Seq (Transaction a)
  -> (Seq Text, Seq (Integer, Int, Int, Seq (Commodity, Integer, Integer)))
  -> TestTree
checkAccount txns (acct, sq)
  = testGroup desc . toList . fmap (checkDay txns acct) $ sq
  where
    desc = "account " <> show (toList $ fmap unpack acct)
