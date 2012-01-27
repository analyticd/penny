module Penny.Total where

import Data.Map ( Map )
import qualified Data.Map as M
import Penny.Bits.Qty ( add, difference )
import qualified Penny.Bits.Qty as Q
import qualified Penny.Bits.Entry as E
import qualified Penny.Bits.Commodity as C
import Data.Monoid ( Monoid, mempty, mappend )
import qualified Penny.Bits.Amount as A

newtype Total = Total (Map C.Commodity Nought)

data Balanced = Balanced
              | Inferable E.Entry
              | NotInferable

isBalanced :: Total -> Balanced
isBalanced (Total m) = M.foldrWithKey f Balanced m where
  f c n b = case n of
    Zero -> b
    (NonZero col) -> case b of
      Balanced -> let
        e = E.Entry dc a
        dc = case drCr col of
          E.Debit -> E.Credit
          E.Credit -> E.Debit
        q = qty col
        a = A.Amount q c
        in Inferable e
      _ -> NotInferable

entryToTotal :: E.Entry -> Total
entryToTotal (E.Entry dc am) = Total $ M.singleton c no where
  c = A.commodity am
  no = NonZero (Column dc (A.qty am))

data Nought = Zero
            | NonZero Column

instance Monoid Nought where
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

data Column = Column { drCr :: E.DrCr
                     , qty :: Q.Qty }

instance Monoid Total where
  mempty = Total M.empty
  mappend (Total t1) (Total t2) =
    Total $ M.unionWith mappend t1 t2

