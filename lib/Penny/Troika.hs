{-# LANGUAGE TemplateHaskell #-}
module Penny.Troika where

import Control.Lens
import Penny.Arrangement
import Penny.Representation
import Penny.Commodity
import Penny.Qty
import Penny.Side

data Troiload
  = QC QtyRepAnyRadix Arrangement
  | Q QtyRepAnyRadix
  | SC QtyNonZero
  | S QtyNonZero
  | UC RepNonNeutralNoSide Side Arrangement
  | U RepNonNeutralNoSide Side
  | C QtyNonZero
  | E QtyNonZero

instance SidedOrNeutral Troiload where
  sideOrNeutral x = case x of
    QC q _ -> sideOrNeutral q
    Q q -> sideOrNeutral q
    SC qnz -> Just . side $ qnz
    S qnz -> Just . side $ qnz
    UC _ s _ -> Just s
    U _ s -> Just s
    C qnz -> Just . side $ qnz
    E qnz -> Just . side $ qnz

instance HasQty Troiload where
  toQty x = case x of
    QC q _ -> toQty q
    Q q -> toQty q
    SC qnz -> toQty qnz
    S qnz -> toQty qnz
    UC rnn s _ -> toQty
      . nilOrBrimScalarAnyRadixToQtyRepAnyRadix s
      . c'NilOrBrimScalarAnyRadix'RepNonNeutralNoSide
      $ rnn
    U rnn s -> toQty
      . nilOrBrimScalarAnyRadixToQtyRepAnyRadix s
      . c'NilOrBrimScalarAnyRadix'RepNonNeutralNoSide
      $ rnn
    C qnz -> toQty qnz
    E qnz -> toQty qnz

data Troika = Troika
  { _commodity :: Commodity
  , _troiload :: Troiload
  }

makeLenses ''Troika

