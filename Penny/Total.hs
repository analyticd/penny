module Penny.Total where

import Data.Map ( Map )
import qualified Data.Map as M
import Penny.Posting (
  Commodity, DrCr, Posting, entry, amount,
  commodity )
import qualified Penny.Posting as P
import Penny.Qty ( add, difference )
import qualified Penny.Qty as Q
import Data.Monoid ( Monoid, mempty, mappend )

newtype Total = Total (Map Commodity Nought)

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

data Column = Column { drCr :: DrCr
                     , qty :: Q.Qty }

total :: Posting -> Total
total p = Total $ M.singleton k (NonZero (Column dc q)) where
  k = commodity . amount . entry $ p
  dc = P.drCr . entry $ p
  q = P.qty . amount . entry $ p

instance Monoid Total where
  mempty = Total M.empty
  mappend (Total t1) (Total t2) =
    Total $ M.unionWith mappend t1 t2
