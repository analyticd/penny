{-# LANGUAGE FlexibleInstances #-}
module Penny.Exch where

import Penny.PluMin
import Penny.Decimal
import Penny.Representation

newtype Exch = Exch Decimal
  deriving (Eq, Ord, Show)

class HasExch a where
  toExch :: a -> Exch

instance HasExch (CenterOrOffCenter (Nil r) (Brim r) PluMin) where
  toExch (Center nil) = Exch . Decimal 0 . toExponent $ nil
  toExch (OffCenter brim p) = Exch . addSign . toDecimal
    . toDecPositive $ brim
    where
      addSign = case p of { Plus -> id; Minus -> negate }

instance HasExch (NilOrBrimPolar r PluMin) where
  toExch (NilOrBrimPolar c) = toExch c

instance HasExch (ExchRep r) where
  toExch (ExchRep n) = toExch n

instance HasExch ExchRepAnyRadix where
  toExch (ExchRepAnyRadix ei) = either toExch toExch ei
