module Penny.Trio where

import qualified Data.Text as X
import qualified Data.Map as M
import Data.Sequence (Seq)

import Penny.Amount
import Penny.Arrangement
import Penny.Balance
import Penny.Commodity
import Penny.Decimal
import Penny.Display
import Penny.Friendly
import Penny.Grammar
  (GrpRadCom, GrpRadPer)
import Penny.Mimode
import Penny.NonEmpty
import Penny.NonZero
import Penny.Polar
import Penny.Rep
import qualified Penny.Troika as K

-- | Given a particular 'Commodity', deliver the correct 'Arrangement'
-- depending on the history of how this commodity was arranged.
arrange
  :: M.Map Commodity (NonEmpty Arrangement)
  -- ^ History map
  -> Commodity
  -> Maybe Arrangement
arrange mp cy = M.lookup cy mp >>= mimode


data Trio
  = QC RepAnyRadix Commodity Arrangement
  -- ^ Specify a quantity and commodity and a corresponding entry is
  -- always recorded.
  --
  -- Preconditions: none
  --
  -- Postconditions: the balance is appropriately affected.

  | Q RepAnyRadix
  -- ^ Specify a quantity only.
  --
  -- Preconditions: there is exactly one commodity in the imbalances.
  -- This commodity is selected to create the entry.
  --
  -- Postconditions: the balance is appropriately affected.


  | SC Pole Commodity
  -- ^ Specify the 'Side' and the 'Commodity'.
  --
  -- Preconditions: the imbalances contain the given commodity, and
  -- its balance has a side opposite the side given here.
  --
  -- Postconditions: the given commodity is eliminated from the
  -- imbalances.


  | S Pole
  -- ^ Specify a 'Side' only.
  --
  -- Preconditions: the imbalances contain exactly one commodity, and
  -- its 'Side' is opposite to the one specified here.
  --
  -- Postconditions: the imbalances is empty.

  | UC BrimScalarAnyRadix Commodity Arrangement
  -- ^ Specify an unsigned abstract quantity only and a 'Commodity'
  -- and how they are arranged.
  --
  -- Preconditions: the imbalances contains the given commodity.
  --
  -- Postconditions: the given commodity in the imbalances either has
  -- its absolute value reduced or it flips to the opposite side.


  | U BrimScalarAnyRadix
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


  deriving Show

data TrioError
  = NoImbalance
  | MultipleImbalance (Commodity, DecNonZero) (Commodity, DecNonZero)
                       [(Commodity, DecNonZero)]
  | CommodityNotFound Commodity
  | BalanceIsSameSide Pole
  | UnsignedTooLarge BrimScalarAnyRadix DecNonZero
  deriving Show

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

    CommodityNotFound cy ->
      [ "Necessary commodity not found in imbalances: " ++ X.unpack cy ]

    BalanceIsSameSide s ->
      [ "Imbalance needs to be on opposite side of given posting,"
      , "but it is on the same side: " ++ (dispSide s)
      ]
    UnsignedTooLarge rnn qnz ->
      [ "Specified quantity of "
        ++ (either display display rnn "") ++ " is larger than "
        ++ "quantity in the imbalance, which is " ++ disp qnz
      ]
    where
      showImbalance (cy, qnz) = X.unpack cy ++ " " ++ disp qnz
      disp = ($ "") . displayDecimalAsQty . fmap nonZeroToInteger
      dispSide side
        | side == debit = "<"
        | otherwise = ">"

trioRendering
  :: Trio
  -> Maybe (Commodity, Arrangement, (Either (Seq GrpRadCom) (Seq GrpRadPer)))
trioRendering tri = case tri of
  QC qr cy ar -> Just (cy, ar, ei)
    where
      ei = case qr of
        Left (Moderate n) -> Left . mayGroupersRadCom $ n
        Left (Extreme (Polarized o _)) -> Left . mayGroupersRadCom $ o
        Right (Moderate n) -> Right . mayGroupersRadPer $ n
        Right (Extreme (Polarized o _)) -> Right . mayGroupersRadPer $ o
  UC rnn cy ar -> Just (cy, ar, ei)
    where
      ei = case rnn of
        Left b -> Left $ mayGroupersRadCom b
        Right b -> Right $ mayGroupersRadPer b
  _ -> Nothing

-- | Extracts the representation from the 'Trio', if there is a
-- representation.  Does not return a 'Side'.
trioRepresentation
  :: Trio
  -> Maybe NilOrBrimScalarAnyRadix
trioRepresentation tri = case tri of
  QC qr _ _ -> Just $ c'NilOrBrimScalarAnyRadix'RepAnyRadix qr
  Q qr -> Just $ c'NilOrBrimScalarAnyRadix'RepAnyRadix qr
  UC rn _ _ -> Just $ c'NilOrBrimScalarAnyRadix'BrimScalarAnyRadix rn
  U rn -> Just $ c'NilOrBrimScalarAnyRadix'BrimScalarAnyRadix rn
  _ -> Nothing


oneCommodity :: Imbalance -> Either TrioError (Commodity, DecNonZero)
oneCommodity (Imbalance imb) = case M.toList imb of
  [] -> Left NoImbalance
  (cy, q):[] -> return (cy, q)
  x:y:xs -> Left $ MultipleImbalance x y xs

notSameSide :: Pole -> Pole -> Either TrioError ()
notSameSide x y
  | x == y = Left $ BalanceIsSameSide x
  | otherwise = return ()

lookupCommodity :: Imbalance -> Commodity -> Either TrioError DecNonZero
lookupCommodity (Imbalance imb) cy = case M.lookup cy imb of
  Nothing -> Left $ CommodityNotFound cy
  Just dnz -> return dnz

rnnIsSmallerAbsoluteValue
  :: BrimScalarAnyRadix
  -> DecNonZero
  -> Either TrioError ()
rnnIsSmallerAbsoluteValue qnr qnz
  | qnr' < qnz' = return ()
  | otherwise = Left $ UnsignedTooLarge qnr qnz
  where
    qnr' = either toDecPositive toDecPositive qnr
    qnz' = toDecPositive qnz

trioToAmount :: Imbalance -> Trio -> Either TrioError Amount

trioToAmount _ (QC qnr cy _) = Right $ Amount cy (toDecimal qnr)

trioToAmount imb (Q qnr) = fmap f $ oneCommodity imb
  where
    f (cy, _) = Amount cy (toDecimal qnr)

trioToAmount imb (SC s cy) = do
  qnz <- lookupCommodity imb cy
  let qtSide = polar qnz
  notSameSide qtSide s
  return
    . Amount cy
    . fmap (negate . nonZeroToInteger)
    $ qnz

trioToAmount imb (S s) = do
  (cy, qnz) <- oneCommodity imb
  let qtSide = polar qnz
  notSameSide s qtSide
  return
    . Amount cy
    . fmap (negate . nonZeroToInteger)
    $ qnz

trioToAmount imb (UC qnr cy _) = do
  qnz <- lookupCommodity imb cy
  return
    . Amount cy
    . fmap nonZeroToInteger
    . fmap (align (opposite . polar $ qnz))
    . fmap c'NonZero'Positive
    . either toDecPositive toDecPositive
    $ qnr

trioToAmount imb (U qnr) = do
  (cy, qnz) <- oneCommodity imb
  rnnIsSmallerAbsoluteValue qnr qnz
  return
    . Amount cy
    . fmap nonZeroToInteger
    . fmap (align (opposite . polar $ qnz))
    . fmap c'NonZero'Positive
    . either toDecPositive toDecPositive
    $ qnr

trioToAmount imb (C cy) = do
  qnz <- lookupCommodity imb cy
  return
    . Amount cy
    . fmap nonZeroToInteger
    . fmap (align (opposite . polar $ qnz))
    $ qnz

trioToAmount imb E = do
  (cy, qnz) <- oneCommodity imb
  return
    . Amount cy
    . fmap nonZeroToInteger
    . fmap (align (opposite . polar $ qnz))
    $ qnz


trioToTroiload
  :: Imbalance
  -> Trio
  -> Either TrioError (K.Troiload, Commodity)

trioToTroiload _ (QC qnr cy ar) = Right (K.QC qnr ar, cy)

trioToTroiload imb (Q qnr) = fmap f $ oneCommodity imb
  where
    f (cy, _) = (K.Q qnr, cy)

trioToTroiload imb (SC s cy) = do
  qnz <- lookupCommodity imb cy
  let qtSide = polar qnz
  notSameSide qtSide s
  return (K.SC qnz, cy)

trioToTroiload imb (S s) = do
  (cy, qnz) <- oneCommodity imb
  let qtSide = polar qnz
  notSameSide s qtSide
  return (K.S qnz, cy)

trioToTroiload imb (UC qnr cy ar) = do
  qnz <- lookupCommodity imb cy
  return (K.UC qnr (opposite . polar $ qnz) ar, cy)

trioToTroiload imb (U qnr) = do
  (cy, qnz) <- oneCommodity imb
  rnnIsSmallerAbsoluteValue qnr qnz
  return (K.U qnr (opposite . polar $ qnz), cy)

trioToTroiload imb (C cy) = do
  qnz <- lookupCommodity imb cy
  return (K.C (fmap (align (opposite . polar $ qnz)) qnz), cy)

trioToTroiload imb E = do
  (cy, qnz) <- oneCommodity imb
  return (K.E (fmap (align (opposite . polar $ qnz)) qnz), cy)

