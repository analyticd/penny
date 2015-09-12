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
  deriving Show

instance SidedOrNeutral Troiload where
  sideOrNeutral x = case x of
    QC q _ -> case q of
      Left rc -> case rc of
        Center _ -> Nothing
        OffCenter _ p -> Just p
      Right rp -> case rp of
        Center _ -> Nothing
        OffCenter _ p -> Just p
    Q q ->  case q of
      Left rc -> case rc of
        Center _ -> Nothing
        OffCenter _ p -> Just p
      Right rp -> case rp of
        Center _ -> Nothing
        OffCenter _ p -> Just p
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

instance SidedOrNeutral Troiquant where
  sideOrNeutral = either sideOrNeutral sideOrNeutral

data Troika = Troika
  { _commodity :: Commodity
  , _troiquant :: Troiquant
  } deriving Show

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
    QC qr ar -> Just (cy, ar, ei)
      where
        ei = case qr of
          Left (Center n) -> Left . mayGroupers $ n
          Left (OffCenter o _) -> Left . mayGroupers $ o
          Right (Center n) -> Right . mayGroupers $ n
          Right (OffCenter o _) -> Right . mayGroupers $ o
    UC rnn _ ar -> Just (cy, ar, ei)
      where
        ei = case rnn of
          Left b -> Left $ mayGroupers b
          Right b -> Right $ mayGroupers b
    _ -> Nothing
  _ -> Nothing
