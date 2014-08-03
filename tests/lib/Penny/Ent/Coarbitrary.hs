module Penny.Ent.Coarbitrary where

import Penny.Ent
import Test.QuickCheck
import Penny.Numbers.Qty.Coarbitrary
import Penny.Common.Coarbitrary
import Penny.Numbers.Abstract.Aggregates.Coarbitrary
import Barecheck.Util

entrio :: Entrio -> Gen b -> Gen b
entrio entro = case entro of
  QC e ar -> varInt 0 . polarEitherRadix e . arrangement ar
  Q e -> varInt 1 . polarEitherRadix e
  SC -> varInt 2
  S -> varInt 3
  UC e a -> varInt 4 . unpolarEitherRadix e . arrangement a
  U e -> varInt 5 . unpolarEitherRadix e
  C -> varInt 6
  E -> varInt 7

ent
  :: (m -> Gen b -> Gen b)
  -> Ent m
  -> Gen b
  -> Gen b
ent g e = qty (entQty e) . commodity (entCommodity e)
  . entrio (entTrio e) . g (entMeta e)
