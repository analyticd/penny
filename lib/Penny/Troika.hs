{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Penny.Troika where

import Control.Lens
import Penny.Arrangement
import Penny.Representation
import Penny.Commodity
import Penny.Qty
import Penny.Side
import qualified Penny.Amount as A
import Data.Sequence (Seq)

data Troiload
  = QC QtyRepAnyRadix Arrangement
  | Q QtyRepAnyRadix
  | SC QtyNonZero
  | S QtyNonZero
  | UC RepNonNeutralNoSide Side Arrangement
  | U RepNonNeutralNoSide Side
  | C QtyNonZero
  | E QtyNonZero
  deriving (Eq, Ord, Show)

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

type Troiquant = Either Troiload Qty

instance HasQty Troiquant where
  toQty = either toQty id

instance SidedOrNeutral Troiquant where
  sideOrNeutral = either sideOrNeutral sideOrNeutral

data Troika = Troika
  { _commodity :: Commodity
  , _troiquant :: Troiquant
  } deriving (Eq, Ord, Show)

instance HasQty Troika where
  toQty (Troika _ tq) = toQty tq

instance SidedOrNeutral Troika where
  sideOrNeutral (Troika _ tq) = sideOrNeutral tq

makeLenses ''Troika

c'Amount'Troika :: Troika -> A.Amount
c'Amount'Troika (Troika cy ei) = A.Amount cy q
  where
    q = either toQty id ei

c'Troika'Amount :: A.Amount -> Troika
c'Troika'Amount (A.Amount cy q) = Troika cy (Right q)

troikaRendering
  :: Troika
  -> Maybe (Commodity, Arrangement, Either (Seq RadCom) (Seq RadPer))
troikaRendering (Troika cy tq) = case tq of
  Left tl -> case tl of
    QC (QtyRepAnyRadix qr) ar -> Just (cy, ar, ei)
      where
        ei = either (Left . mayGroupers) (Right . mayGroupers) qr
    UC (RepNonNeutralNoSide ei) _ ar ->
      Just (cy, ar, either (Left . mayGroupers) (Right . mayGroupers) ei)
    _ -> Nothing
  _ -> Nothing
