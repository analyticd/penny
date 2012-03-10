module PennyTest.Lincoln.Balance where

import Control.Applicative ((<$>), (<*>))
import qualified Data.Decimal as D
import qualified Data.Map as M
import Data.Monoid (mappend)
import qualified Penny.Lincoln.Balance as B
import qualified Penny.Lincoln.Bits as Bits
import PennyTest.Lincoln.Bits ()
import Test.QuickCheck (Arbitrary, arbitrary, oneof, Gen,
                        elements, suchThat, listOf)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework (Test, testGroup)


instance Arbitrary B.Column where
  arbitrary = B.Column <$> arbitrary <*> arbitrary

instance Arbitrary B.BottomLine where
  arbitrary = oneof [ return B.Zero
                    , B.NonZero <$> arbitrary ]

{-
-- | Adding the given entry to an inferable balance results in a
-- balance that is balanced.
prop_addInference :: Bits.Commodity -> B.Nought -> Bool
prop_addInference c n = let
  bal = B.Balance (M.singleton c n)
  en = case B.isBalanced bal of
    B.Balanced -> Nothing
    B.Inferable e -> Just e
    B.NotInferable -> Nothing
  bal' e = mappend bal (B.entryToBalance e)
  isGood b = case B.isBalanced b of
    B.Balanced -> True
    _ -> False
  in case en of
    Nothing -> case n of
      B.Zero -> True
      _ -> False
    Just e -> if isGood (bal' e) then True else False

test_addInference :: Test
test_addInference = testProperty s prop_addInference where
  s = "Adding inference results in balanced"

newtype BalancedBal = BalancedBal { unBalancedBal :: B.Balance }
                      deriving (Eq, Show)

-- | Given an entry, return a list of entries that will balance
-- it. List might be zero in length because an entry whose quantity is
-- zero does not need another entry to balance it.
balancers :: Bits.Entry -> Gen [Bits.Entry]
balancers e = let
  qtyDec = Bits.unQty . Bits.qty . Bits.amount $ e
  opposite = case Bits.drCr e of
    Bits.Debit -> Bits.Credit
    Bits.Credit -> Bits.Debit
  cmdty = Bits.commodity . Bits.amount $ e
  mkEntry q = Bits.Entry opposite
              (Bits.Amount (Bits.partialNewQty q) cmdty)
  qtys = listOf (suchThat arbitrary (>= 0))
         >>= return . D.allocate qtyDec
  entries' = (map mkEntry) <$> qtys
  in if qtyDec == 0
     then elements [[], [mkEntry qtyDec]]
     else entries'

-}
tests :: Test
tests = testGroup "Balance" []
