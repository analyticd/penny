module Penny.Lincoln.Balance (
  Balance,
  unBalance,
  Balanced(Balanced, Inferable, NotInferable),
  isBalanced,
  entryToBalance,
  addBalances,
  removeZeroCommodities,
  BottomLine(Zero, NonZero),
  Column(Column, drCr, qty)
  ) where

import Data.Map ( Map )
import qualified Data.Map as M
import Data.Monoid ( Monoid, mempty, mappend )
import qualified Data.Semigroup as Semi

import Penny.Lincoln.Bits (
  add, difference, Difference(LeftBiggerBy, RightBiggerBy, Equal))
import qualified Penny.Lincoln.Bits as B

-- | A balance summarizes several entries. You do not create a Balance
-- directly. Instead, use 'entryToBalance'.
newtype Balance = Balance (Map B.Commodity BottomLine)
                  deriving (Show, Eq)

-- | Returns a map where the keys are the commodities in the balance
-- and the values are the balance for each commodity. If there is no
-- balance at all, this map can be empty.
unBalance :: Balance -> Map B.Commodity BottomLine
unBalance (Balance m) = m

-- | Returned by 'isBalanced'.
data Balanced = Balanced
              | Inferable B.Entry
              | NotInferable
              deriving (Show, Eq)

-- | Is this balance balanced?
isBalanced :: Balance -> Balanced
isBalanced (Balance m) = M.foldrWithKey f Balanced m where
  f c n b = case n of
    Zero -> b
    (NonZero col) -> case b of
      Balanced -> let
        e = B.Entry dc a
        dc = case drCr col of
          B.Debit -> B.Credit
          B.Credit -> B.Debit
        q = qty col
        a = B.Amount q c
        in Inferable e
      _ -> NotInferable

-- | Converts an Entry to a Balance.
entryToBalance :: B.Entry -> Balance
entryToBalance (B.Entry dc am) = Balance $ M.singleton c no where
  c = B.commodity am
  no = NonZero (Column dc (B.qty am))

data BottomLine = Zero
            | NonZero Column
            deriving (Show, Eq)

instance Monoid BottomLine where
  mempty = Zero
  mappend n1 n2 = case (n1, n2) of
    (Zero, Zero) -> Zero
    (Zero, (NonZero c)) -> NonZero c
    ((NonZero c), Zero) -> NonZero c
    ((NonZero c1), (NonZero c2)) ->
      let (Column dc1 q1) = c1
          (Column dc2 q2) = c2
      in if dc1 == dc2
         then NonZero $ Column dc1 (q1 `add` q2)
         else case difference q1 q2 of
           LeftBiggerBy diff ->
             NonZero $ Column dc1 diff
           RightBiggerBy diff ->
             NonZero $ Column dc2 diff
           Equal -> Zero

data Column = Column { drCr :: B.DrCr
                     , qty :: B.Qty }
              deriving (Show, Eq)

-- | Add two Balances together. Commodities are never removed from the
-- balance, even if their balance is zero. Instead, they are left in
-- the balance. Sometimes you want to know that a commodity was in the
-- account but its balance is now zero.
addBalances :: Balance -> Balance -> Balance
addBalances (Balance t1) (Balance t2) =
    Balance $ M.unionWith mappend t1 t2

instance Semi.Semigroup Balance where
  (<>) = addBalances

instance Monoid Balance where
  mempty = Balance M.empty
  mappend = addBalances

-- | Removes zero balances from a Balance.
removeZeroCommodities :: Balance -> Balance
removeZeroCommodities (Balance m) =
  let p b = case b of
        Zero -> False
        _ -> True
      m' = M.filter p m
  in Balance m'
