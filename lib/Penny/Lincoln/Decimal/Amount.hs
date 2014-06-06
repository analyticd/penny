{-# LANGUAGE MultiParamTypeClasses #-}
module Penny.Lincoln.Decimal.Amount where

import Penny.Lincoln.Decimal.Abstract

-- | An amount is either an 'Abstract' or a 'Concrete'.  All
-- 'Amount' embody a decimal number along with a 'Lane' (that is, a
-- 'Penny.Lincoln.Decimal.Side.Debit', a
-- 'Penny.Lincoln.Decimal.Side.Credit', or neither if the number is
-- zero.)  Use 'lane' to determine the 'Lane' of an 'Amount'.  All
-- 'Amount' may be transformed to a 'Concrete' using 'concrete'; you
-- may perform arithmetic only on a 'Concrete'.  Transform a
-- 'Concrete' to an 'Abstract' (which is the only form that may be
-- rendered as a string) using the functions in
-- "Penny.Lincoln.Decimal.Represent".
data Amount p c
  = AAbstract (Abstract p)
  | AConcrete c
  deriving (Eq, Ord, Show)

