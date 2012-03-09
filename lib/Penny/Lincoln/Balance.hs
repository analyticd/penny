module Penny.Lincoln.Balance ( 
  Balance,
  unBalance,
  Balanced(Balanced, Inferable, NotInferable),
  isBalanced,
  entryToBalance,
  BottomLine(Zero, NonZero),
  Column,
  drCr,
  qty) where

import Data.Map ( Map )
import qualified Data.Map as M
import Data.Monoid ( Monoid, mempty, mappend )

import Penny.Lincoln.Bits (add, difference)
import qualified Penny.Lincoln.Bits as B

-- | A balance summarizes several entries. You do not create a Balance
-- directly. Instead, use 'entryToBalance'.
newtype Balance = Balance (Map B.Commodity BottomLine)
                  deriving (Show, Eq)

-- | The map returned by unBalance is never empty.
unBalance :: Balance -> Map B.Commodity BottomLine
unBalance (Balance m) = m

-- | Returned by 'isBalanced'.
data Balanced = Balanced
              | Inferable B.Entry
              | NotInferable
              deriving (Show, Eq)

-- | Is this balance balanced? A single key-value pair is balanced if
-- its BottomLine is Zero. A key-value pair is not balanced if its BottomLine
-- is NonZero, even if the Column contained therein has a Qty of
-- zero. Instead, the column would require an equalizing
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
         else if q1 == q2
              then Zero
              else let
                q' = q1 `difference` q2
                dc' = if q1 > q2 then dc1 else dc2
                in NonZero $ Column dc' q'

data Column = Column { drCr :: B.DrCr
                     , qty :: B.Qty }
              deriving (Show, Eq)

instance Monoid Balance where
  mempty = Balance M.empty
  mappend (Balance t1) (Balance t2) =
    Balance $ M.unionWith mappend t1 t2

