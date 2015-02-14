module Penny.Lincoln.Trio where

import Penny.Lincoln.Ent
import Penny.Lincoln.Decimal
import Penny.Lincoln.Commodity
import Penny.Lincoln.Balances
import Penny.Lincoln.Side
import Penny.Lincoln.Rep
import Penny.Lincoln.Qty
import Penny.Lincoln.Offset
import Penny.Lincoln.Friendly
import qualified Data.Map as M
import qualified Data.Text as X

data Orient
  = CommodityOnLeft
  | CommodityOnRight
  deriving (Eq, Ord, Show)

newtype SpaceBetween = SpaceBetween Bool
  deriving (Eq, Ord, Show)

data Arrangement = Arrangement Orient SpaceBetween
  deriving (Eq, Ord, Show)

data Trio
  = QC QtyRepAnyRadix Commodity Arrangement
  -- ^ Specify a quantity and commodity and a corresponding entry is
  -- always recorded.
  --
  -- Preconditions: none
  --
  -- Postconditions: the balance is appropriately affected.

  | Q QtyRepAnyRadix
  -- ^ Specify a quantity only.
  --
  -- Preconditions: there is exactly one commodity in the imbalances.
  -- This commodity is selected to create the entry.
  --
  -- Postconditions: the balance is appropriately affected.


  | SC Side Commodity
  -- ^ Specify the 'Side' and the 'Commodity'.
  --
  -- Preconditions: the imbalances contain the given commodity, and
  -- its balance has a side opposite the side given here.
  --
  -- Postconditions: the given commodity is eliminated from the
  -- imbalances.


  | S Side
  -- ^ Specify a 'Side' only.
  --
  -- Preconditions: the imbalances contain exactly one commodity, and
  -- its 'Side' is opposite to the one specified here.
  --
  -- Postconditions: the imbalances is empty.

  | UC RepNonNeutralNoSide Commodity Arrangement
  -- ^ Specify an unsigned abstract quantity only and a 'Commodity'
  -- and how they are arranged.
  --
  -- Preconditions: the imbalances contains the given commodity.
  --
  -- Postconditions: the given commodity in the imbalances either has
  -- its absolute value reduced or it flips to the opposite side.


  | U RepNonNeutralNoSide
  -- ^ Specify an unsigned quantity only.
  --
  -- Preconditions: the imbalances contains exactly one commodity, and
  -- its absolute value is greater than or equal to than the given amount.
  --
  -- Postconditions: the given commodity in the imbalances is reduced
  -- by the amount given.


  | C Commodity
  -- ^ Specify a 'Commodity' only.
  --
  -- Preconditions: the given commodity is in the imbalances.
  --
  -- Postconditions: the given commodity is eliminated from the
  -- imbalances.


  | E
  -- ^ Specify nothing at all.
  --
  -- Preconditions: there is exactly one commodity in the imbalances.
  --
  -- Postconditions: the imbalances is empty.


  deriving (Eq, Ord, Show)

data TrioError
  = NoImbalances
  | MultipleImbalances (Commodity, QtyNonZero) (Commodity, QtyNonZero)
                       [(Commodity, QtyNonZero)]
  | CommodityNotFound Commodity
  | BalanceIsSameSide Side
  | UnsignedTooLarge RepNonNeutralNoSide QtyNonZero
  deriving (Eq, Ord, Show)


instance Friendly TrioError where
  friendly te = case te of
    NoImbalances ->
      [ "The posting you gave requires there to be a current imbalance,"
      , "but the postings are perfectly balanced."
      ]
    MultipleImbalances i1 i2 is ->
      [ "The posting you gave requires there to be exactly one commodity"
      , "that is not balanced, but there are multiple imbalances:"
      , showImbalance i1
      , showImbalance i2
      ] ++ map showImbalance is

    CommodityNotFound (Commodity cy) ->
      [ "Necessary commodity not found in imbalances: " ++ X.unpack cy ]

    BalanceIsSameSide s ->
      [ "Imbalances needs to be on opposite side of given posting,"
      , "but it is on the same side: " ++ (display s "")
      ]
    UnsignedTooLarge rnn qnz ->
      [ "Specified quantity of " ++ (display rnn "") ++ " is larger than "
        ++ "quantity in the imbalance, which is " ++ disp qnz
      ]
    where
      showImbalance (Commodity cy, qnz) = X.unpack cy ++ " " ++ disp qnz
      disp = displayQtyNonZero

qtyAndCommodityToEnt
  :: QtyRepAnyRadix
  -> Commodity
  -> a
  -> Ent a
qtyAndCommodityToEnt qnr cy a = Ent (toQty qnr) cy a

toEnt :: Imbalances -> Trio -> Either TrioError (a -> Ent a)

toEnt _ (QC qnr cy _) = Right $ qtyAndCommodityToEnt qnr cy

toEnt imb (Q qnr) = fmap f $ oneCommodity imb
  where
    f (cy, _) = Ent (toQty qnr) cy

toEnt imb (SC s cy) = do
  qnz <- lookupCommodity imb cy
  let qtSide = qtyNonZeroSide qnz
  notSameSide qtSide s
  return $ Ent (toQty . offset $ qnz) cy

toEnt imb (S s) = do
  (cy, qnz) <- oneCommodity imb
  let qtSide = qtyNonZeroSide qnz
  notSameSide s qtSide
  return $ Ent (toQty . offset $ qnz) cy


toEnt imb (UC qnr cy _) = do
  qnz <- lookupCommodity imb cy
  let q = decPositiveToQty (offset . qtyNonZeroSide $ qnz)
        . toDecPositive $ qnr
  return $ Ent q cy

toEnt imb (U qnr) = do
  (cy, qnz) <- oneCommodity imb
  qnrIsSmallerAbsoluteValue qnr qnz
  let q = decPositiveToQty (offset . qtyNonZeroSide $ qnz)
        . toDecPositive $ qnr
  return $ Ent q cy

toEnt imb (C cy) = do
  qnz <- lookupCommodity imb cy
  let q = toQty . offset $ qnz
  return $ Ent q cy

toEnt imb E = do
  (cy, qnz) <- oneCommodity imb
  let q = toQty . offset $ qnz
  return $ Ent q cy


qnrIsSmallerAbsoluteValue
  :: RepNonNeutralNoSide
  -> QtyNonZero
  -> Either TrioError ()
qnrIsSmallerAbsoluteValue qnr qnz
  | qnr' < qnz' = return ()
  | otherwise = Left $ UnsignedTooLarge qnr qnz
  where
    qnr' = Semantic . toDecimal . toDecPositive $ qnr
    qnz' = Semantic . toDecimal . toDecPositive
      . (\(QtyNonZero dnz) -> dnz) $ qnz

notSameSide :: Side -> Side -> Either TrioError ()
notSameSide x y
  | x == y = Left $ BalanceIsSameSide x
  | otherwise = return ()

oneCommodity :: Imbalances -> Either TrioError (Commodity, QtyNonZero)
oneCommodity (Imbalances imb) = case M.toList imb of
  [] -> Left NoImbalances
  (cy, q):[] -> return (cy, q)
  x:y:xs -> Left $ MultipleImbalances x y xs

lookupCommodity :: Imbalances -> Commodity -> Either TrioError QtyNonZero
lookupCommodity (Imbalances imb) cy = case M.lookup cy imb of
  Nothing -> Left $ CommodityNotFound cy
  Just dnz -> return dnz
