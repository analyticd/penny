module Penny.Trio where

import Penny.Amount
import Penny.Ent
import Penny.Decimal
import Penny.Display
import Penny.Commodity
import Penny.Balance
import Penny.Side
import Penny.Representation
import Penny.Qty
import Penny.Offset
import Penny.Friendly
import Penny.NonEmpty
import qualified Data.Map as M
import qualified Data.Text as X
import Data.Sequence (Seq)
import Penny.Mimode
import Penny.Semantic

data Orient
  = CommodityOnLeft
  | CommodityOnRight
  deriving (Eq, Ord, Show)

newtype SpaceBetween = SpaceBetween Bool
  deriving (Eq, Ord, Show)

data Arrangement = Arrangement Orient SpaceBetween
  deriving (Eq, Ord, Show)

-- | Given a particular 'Commodity', deliver the correct 'Arrangement'
-- depending on the history of how this commodity was arranged.
arrange
  :: M.Map Commodity (NonEmpty Arrangement)
  -- ^ History map
  -> Commodity
  -> Maybe Arrangement
arrange mp cy = M.lookup cy mp >>= mimode


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
  = NoImbalance
  | MultipleImbalance (Commodity, QtyNonZero) (Commodity, QtyNonZero)
                       [(Commodity, QtyNonZero)]
  | CommodityNotFound Commodity
  | BalanceIsSameSide Side
  | UnsignedTooLarge RepNonNeutralNoSide QtyNonZero
  deriving (Eq, Ord, Show)


instance Friendly TrioError where
  friendly te = case te of
    NoImbalance ->
      [ "The posting you gave requires there to be a current imbalance,"
      , "but the postings are perfectly balanced."
      ]
    MultipleImbalance i1 i2 is ->
      [ "The posting you gave requires there to be exactly one commodity"
      , "that is not balanced, but there are multiple imbalances:"
      , showImbalance i1
      , showImbalance i2
      ] ++ map showImbalance is

    CommodityNotFound (Commodity cy) ->
      [ "Necessary commodity not found in imbalances: " ++ X.unpack cy ]

    BalanceIsSameSide s ->
      [ "Imbalance needs to be on opposite side of given posting,"
      , "but it is on the same side: " ++ (display s "")
      ]
    UnsignedTooLarge rnn qnz ->
      [ "Specified quantity of " ++ (display rnn "") ++ " is larger than "
        ++ "quantity in the imbalance, which is " ++ disp qnz
      ]
    where
      showImbalance (Commodity cy, qnz) = X.unpack cy ++ " " ++ disp qnz
      disp = displayQtyNonZero

trioRendering
  :: Trio
  -> Maybe (Commodity, Arrangement, (Either (Seq RadCom) (Seq RadPer)))
trioRendering tri = case tri of
  QC (QtyRepAnyRadix qr) cy ar -> Just (cy, ar, ei)
    where
      ei = either (Left . mayGroupers) (Right . mayGroupers) qr
  UC (RepNonNeutralNoSide ei) cy ar ->
    Just (cy, ar, either (Left . mayGroupers) (Right . mayGroupers) ei)
  _ -> Nothing

-- | Extracts the representation from the 'Trio', if there is a
-- representation.  Does not return a 'Side'.
trioRepresentation
  :: Trio
  -> Maybe NilOrBrimScalarAnyRadix
trioRepresentation tri = case tri of
  QC qr _ _ -> Just $ c'NilOrBrimScalarAnyRadix'QtyRepAnyRadix qr
  Q qr -> Just $ c'NilOrBrimScalarAnyRadix'QtyRepAnyRadix qr
  UC rn _ _ -> Just $ c'NilOrBrimScalarAnyRadix'RepNonNeutralNoSide rn
  U rn -> Just $ c'NilOrBrimScalarAnyRadix'RepNonNeutralNoSide rn
  _ -> Nothing


qtyAndCommodityToEnt
  :: QtyRepAnyRadix
  -> Commodity
  -> a
  -> Ent a
qtyAndCommodityToEnt qnr cy a = Ent (Amount cy (toQty qnr)) a

toEnt :: Imbalance -> Trio -> Either TrioError (a -> Ent a)

toEnt _ (QC qnr cy _) = Right $ qtyAndCommodityToEnt qnr cy

toEnt imb (Q qnr) = fmap f $ oneCommodity imb
  where
    f (cy, _) = Ent (Amount cy (toQty qnr))

toEnt imb (SC s cy) = do
  qnz <- lookupCommodity imb cy
  let qtSide = qtyNonZeroSide qnz
  notSameSide qtSide s
  return $ Ent (Amount cy (toQty . offset $ qnz))

toEnt imb (S s) = do
  (cy, qnz) <- oneCommodity imb
  let qtSide = qtyNonZeroSide qnz
  notSameSide s qtSide
  return $ Ent (Amount cy (toQty . offset $ qnz))


toEnt imb (UC qnr cy _) = do
  qnz <- lookupCommodity imb cy
  let q = decPositiveToQty (offset . qtyNonZeroSide $ qnz)
        . toDecPositive $ qnr
  return $ Ent (Amount cy q)

toEnt imb (U qnr) = do
  (cy, qnz) <- oneCommodity imb
  qnrIsSmallerAbsoluteValue qnr qnz
  let q = decPositiveToQty (offset . qtyNonZeroSide $ qnz)
        . toDecPositive $ qnr
  return $ Ent (Amount cy q)

toEnt imb (C cy) = do
  qnz <- lookupCommodity imb cy
  let q = toQty . offset $ qnz
  return $ Ent (Amount cy q)

toEnt imb E = do
  (cy, qnz) <- oneCommodity imb
  let q = toQty . offset $ qnz
  return $ Ent (Amount cy q)


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

oneCommodity :: Imbalance -> Either TrioError (Commodity, QtyNonZero)
oneCommodity (Imbalance imb) = case M.toList imb of
  [] -> Left NoImbalance
  (cy, q):[] -> return (cy, q)
  x:y:xs -> Left $ MultipleImbalance x y xs

lookupCommodity :: Imbalance -> Commodity -> Either TrioError QtyNonZero
lookupCommodity (Imbalance imb) cy = case M.lookup cy imb of
  Nothing -> Left $ CommodityNotFound cy
  Just dnz -> return dnz
