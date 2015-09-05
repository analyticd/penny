{-# LANGUAGE TemplateHaskell #-}
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

-- type Troiquant = Either Troiload Qty

newtype Troiquant = Troiquant (Either Troiload Qty)
  deriving (Eq, Ord, Show)

instance HasQty Troiquant where
  toQty (Troiquant ei) = either toQty id ei

data Troimount = Troimount
  { _commodity :: Commodity
  , _troiquant :: Troiquant
  } deriving (Eq, Ord, Show)

instance HasQty Troimount where
  toQty (Troimount _ tq) = toQty tq

makeLenses ''Troimount

c'Amount'Troimount :: Troimount -> A.Amount
c'Amount'Troimount (Troimount cy (Troiquant ei)) = A.Amount cy q
  where
    q = either toQty id ei

troimountRendering
  :: Troimount
  -> Maybe (Commodity, Arrangement, Either (Seq RadCom) (Seq RadPer))
troimountRendering (Troimount cy (Troiquant tq)) = case tq of
  Left tl -> case tl of
    QC (QtyRepAnyRadix qr) ar -> Just (cy, ar, ei)
      where
        ei = either (Left . mayGroupers) (Right . mayGroupers) qr
    UC (RepNonNeutralNoSide ei) _ ar ->
      Just (cy, ar, either (Left . mayGroupers) (Right . mayGroupers) ei)
    _ -> Nothing
  _ -> Nothing
