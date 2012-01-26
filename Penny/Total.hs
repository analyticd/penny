module Penny.Total where

import Data.Map ( Map )
import qualified Data.Map as M
import Penny.Bits.Qty ( add, difference )
import qualified Penny.Bits.Qty as Q
import qualified Penny.Bits.Entry as E
import qualified Penny.Bits.Commodity as C
import Data.Monoid ( Monoid, mempty, mappend )
import qualified Penny.Bits.Price as P
import qualified Penny.Bits.Amount as A

newtype Total = Total (Map C.Commodity Nought)

isBalanced :: Total -> Bool
isBalanced (Total m) = M.fold f True m where
  f _ False = False
  f (NonZero _) _ = False
  f Zero e = e

valueToTotal :: P.Value -> Total
valueToTotal (P.Value dc am) = Total $ M.singleton c no where
  c = A.commodity am
  no = NonZero (Column dc (A.qty am))

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

