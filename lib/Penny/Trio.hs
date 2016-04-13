module Penny.Trio where

import qualified Control.Lens as Lens
import Data.Foldable (toList)
import qualified Data.Text as X
import qualified Data.Map as M
import Data.Sequence (Seq)

import Penny.Amount
import Penny.Arrangement
import Penny.Balance
import Penny.Commodity
import Penny.Decimal
import Penny.Friendly
import Penny.Copper.Terminalizers
import Penny.Copper.Types
  (GrpRadCom, GrpRadPer)
import Penny.Mimode
import Penny.NonEmpty
import Penny.NonZero
import Penny.Polar
import qualified Penny.Positive as Pos
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

  | UC BrimAnyRadix Commodity Arrangement
  -- ^ Specify an unsigned abstract quantity only and a 'Commodity'
  -- and how they are arranged.
  --
  -- Preconditions: the imbalances contains the given commodity.
  --
  -- Postconditions: the given commodity in the imbalances either has
  -- its absolute value reduced or it flips to the opposite side.

  | NC NilAnyRadix Commodity Arrangement
  -- ^ Specify a nil quantity and a 'Commodity' and how they are
  -- arranged.
  --
  -- Preconditions: None.
  --
  -- Postconditions: the given commodity and quantity is added to
  -- the imbalances map.


  | US BrimAnyRadix
  -- ^ Specify an unsigned quantity only.
  --
  -- Preconditions: the imbalances contains exactly one commodity, and
  -- its absolute value is greater than or equal to than the given amount.
  --
  -- Postconditions: the given commodity in the imbalances is reduced
  -- by the amount given.

  | UU NilAnyRadix
  -- ^ Specify a nil quantity only.
  --
  -- Preconditions: the imbalances contain exactly one commodity.
  --
  -- Postconditions: no change in imbalances.

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
  | UnsignedTooLarge BrimAnyRadix DecNonZero
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
        ++ (toList . fmap fst . either t'BrimRadCom t'BrimRadPer $ rnn)
        ++ " is larger than "
        ++ "quantity in the imbalance, which is " ++ disp qnz
      ]
    where
      showImbalance (cy, qnz) = X.unpack cy ++ " " ++ disp qnz
      disp = ($ "") . displayDecimalAsQty . fmap c'Integer'NonZero
      dispSide side
        | side == debit = "<"
        | otherwise = ">"

trioRendering
  :: Trio
  -> Maybe (Commodity, Arrangement,
        Either (Seq (GrpRadCom Char ())) (Seq (GrpRadPer Char ())))
trioRendering tri = case tri of
  QC qr cy ar -> Just (cy, ar, ei)
    where
      ei = groupers'RepAnyRadix qr
  UC rnn cy ar -> Just (cy, ar, ei)
    where
      ei = groupers'BrimAnyRadix rnn
  _ -> Nothing

-- | Extracts the representation from the 'Trio', if there is a
-- representation.  Does not return a 'Side'.
trioRepresentation
  :: Trio
  -> Maybe NilOrBrimAnyRadix
trioRepresentation tri = case tri of
  QC qr _ _ -> Just $ c'NilOrBrimAnyRadix'RepAnyRadix qr
  Q qr -> Just $ c'NilOrBrimAnyRadix'RepAnyRadix qr
  UC rn _ _ -> Just $ c'NilOrBrimAnyRadix'BrimAnyRadix rn
  US brim -> Just $ c'NilOrBrimAnyRadix'BrimAnyRadix brim
  UU nil -> Just $ c'NilOrBrimAnyRadix'NilAnyRadix nil
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
  :: BrimAnyRadix
  -> DecNonZero
  -> Either TrioError ()
rnnIsSmallerAbsoluteValue qnr qnz
  | _coefficient qnr'' < _coefficient qnz'' = return ()
  | otherwise = Left $ UnsignedTooLarge qnr qnz
  where
    qnr' = fmap Pos.c'Integer'Positive . c'DecPositive'BrimAnyRadix $ qnr
    qnz' = c'Decimal'DecNonZero qnz
    (qnr'', qnz'') = equalizeExponents qnr' qnz'

trioToAmount :: Imbalance -> Trio -> Either TrioError Amount

trioToAmount _ (QC qnr cy _) = Right $ Amount cy (c'Decimal'RepAnyRadix qnr)

trioToAmount imb (Q qnr) = fmap f $ oneCommodity imb
  where
    f (cy, _) = Amount cy (c'Decimal'RepAnyRadix qnr)

trioToAmount imb (SC s cy) = do
  qnz <- lookupCommodity imb cy
  let qtSide = Lens.view (coefficient . pole) qnz
  notSameSide qtSide s
  return
    . Amount cy
    . fmap (Prelude.negate . c'Integer'NonZero)
    $ qnz

trioToAmount imb (S s) = do
  (cy, qnz) <- oneCommodity imb
  let qtSide = Lens.view (coefficient . pole) qnz
  notSameSide s qtSide
  return
    . Amount cy
    . fmap (Prelude.negate . c'Integer'NonZero)
    $ qnz

trioToAmount imb (UC qnr cy _) = do
  qnz <- lookupCommodity imb cy
  return
    . Amount cy
    . Lens.over coefficient c'Integer'NonZero
    . Lens.set (coefficient.pole) (Lens.view (coefficient.pole) qnz)
    . fmap c'NonZero'Positive
    . c'DecPositive'BrimAnyRadix
    $ qnr

trioToAmount _ (NC nilAnyRadix cy _) = Right (Amount cy qty)
  where
    qty = fmap (const 0) . c'DecZero'NilAnyRadix $ nilAnyRadix

trioToAmount imb (US qnr) = do
  (cy, qnz) <- oneCommodity imb
  rnnIsSmallerAbsoluteValue qnr qnz
  return
    . Amount cy
    . Lens.over coefficient c'Integer'NonZero
    . Lens.set (coefficient.pole) (Lens.view (coefficient.pole) qnz)
    . fmap c'NonZero'Positive
    . c'DecPositive'BrimAnyRadix
    $ qnr

trioToAmount imb (UU nil) = do
  (cy, _) <- oneCommodity imb
  return (Amount cy (fmap (const 0) (c'DecZero'NilAnyRadix nil)))

trioToAmount imb (C cy) = do
  qnz <- lookupCommodity imb cy
  return
    . Amount cy
    . fmap c'Integer'NonZero
    . Lens.set (coefficient.pole) (Lens.view (coefficient.pole) qnz)
    $ qnz

trioToAmount imb E = do
  (cy, qnz) <- oneCommodity imb
  return
    . Amount cy
    . fmap c'Integer'NonZero
    . Lens.set (coefficient.pole) (Lens.view (coefficient.pole) qnz)
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
  let qtSide = Lens.view (coefficient . pole) qnz
  notSameSide qtSide s
  return (K.SC qnz, cy)

trioToTroiload imb (S s) = do
  (cy, qnz) <- oneCommodity imb
  let qtSide = Lens.view (coefficient . pole) qnz
  notSameSide s qtSide
  return (K.S qnz, cy)

trioToTroiload imb (UC qnr cy ar) = do
  qnz <- lookupCommodity imb cy
  return (K.UC qnr (opposite . Lens.view (coefficient . pole) $ qnz) ar, cy)

trioToTroiload _ (NC nilAnyRadix cy ar) = Right (troiload, cy)
  where
    troiload = K.NC nilAnyRadix ar

trioToTroiload imb (US brim) = do
  (cy, qnz) <- oneCommodity imb
  rnnIsSmallerAbsoluteValue brim qnz
  let pole' = Lens.view (coefficient . pole . Lens.to opposite) qnz
  return (K.US brim pole', cy)

trioToTroiload imb (UU nil) = do
  (cy, _) <- oneCommodity imb
  return (K.UU nil, cy)

trioToTroiload imb (C cy) = do
  qnz <- lookupCommodity imb cy
  let qnz' = Lens.over (coefficient . pole) opposite qnz
  return (K.C qnz', cy)

trioToTroiload imb E = do
  (cy, qnz) <- oneCommodity imb
  return (K.E (Lens.over (coefficient . pole) opposite qnz), cy)

