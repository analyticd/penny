module Penny.Core.Trio where

import qualified Penny.Core.Commodity as Commodity
import qualified Penny.Core.Arrangement as Arrangement
import qualified Penny.Core.Concrete as Concrete
import qualified Penny.Core.Side as Side
import qualified Penny.Core.Muddy as Muddy
import qualified Penny.Core.Philly as Philly
import qualified Penny.Core.Pebble as Pebble
import qualified Penny.Core.Ent as Ent
import qualified Penny.Core.Imbalances as Imbalances
import qualified Penny.Core.Trio.Error as Error
import qualified Penny.Core.Qty as Qty
import qualified Penny.Core.Quark as Quark
import qualified Penny.Core.Quant as Quant
import qualified Data.Map as M

-- | When building entries using the "Penny.Ents" module, you may
-- specify any of the 'Side.T', 'Commodity.T', a signed abstract quantity,
-- or an unsigned abstract quantity.  Which of these you specify,
-- combined with the running balance of the postings in the
-- Ents, determines whether an Ent is created or an error
-- occurs.
--
-- The order of the postings is important because Penny keeps a
-- running balance of the postings it has seen so far.  If some
-- entries must be inferred, the inferred value depends on the balance
-- of the postings seen so far.
--
-- The naming scheme is cryptic: @Q@ stands for a signed abstract
-- quantity; @C@ for a 'Commodity', @S@ for @Side@, @U@ for unsigned
-- abstract quantity 'Z' for @NZGrouped@, 'C' for @Commodity@, and 'E'
-- for @Empty@.

data T
  = QC Muddy.T Commodity.T Arrangement.T
  -- ^ Specify a quantity, commodity, and how they are arranged, and a
  -- corresponding entry is always recorded.
  --
  -- Preconditions: none
  --
  -- Postconditions: the balance is appropriately affected.


  | Q Muddy.T
  -- ^ Specify a quantity only.
  --
  -- Preconditions: there is exactly one commodity in the imbalances.
  -- This commodity is selected to create the entry.
  --
  -- Postconditions: the balance is appropriately affected.

  | SC Side.T Commodity.T
  -- ^ Specify the 'Side' and the 'Commodity'.
  --
  -- Preconditions: the imbalances contain the given commodity, and
  -- its balance has a side opposite the side given here.
  --
  -- Postconditions: the given commodity is eliminated from the
  -- imbalances.

  | S Side.T
  -- ^ Specify a 'Side' only.
  --
  -- Preconditions: the imbalances contain exactly one commodity, and
  -- its 'Side' is opposite to the one specified here.
  --
  -- Postconditions: the imbalances is empty.


  | UC Philly.T Commodity.T Arrangement.T
  -- ^ Specify an unsigned abstract quantity only and a 'Commodity'
  -- and how they are arranged.
  --
  -- Preconditions: the imbalances contains the given commodity.
  --
  -- Postconditions: the given commodity in the imbalances either has
  -- its absolute value reduced or it flips to the opposite side.

  | U Philly.T
  -- ^ Specify an unsigned quantity only.
  --
  -- Preconditions: the imbalances contains exactly one commodity, and
  -- its absolute value is greater than or equal to than the given amount.
  --
  -- Postconditions: the given commodity in the imbalances is reduced
  -- by the amount given.

  | C Commodity.T
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

muddyCommodityToEnt :: Muddy.T -> Commodity.T -> a -> Ent.T a
muddyCommodityToEnt muddy cy a = Ent.T q cy a
  where
    q = Qty.fromCement . Pebble.toCement . Muddy.toPebble $ muddy

toEnt :: Imbalances.T -> T -> a -> Either Error.T (Ent.T a)

toEnt _ (QC muddy cy _) a = Right $ muddyCommodityToEnt muddy cy a

toEnt imb (Q muddy) a = fmap f $ oneCommodity imb
  where
    f (cy, _) = Ent.T q cy a
      where
        q = Qty.fromCement . Pebble.toCement . Muddy.toPebble $ muddy

toEnt imb (SC s cy) a = do
  qk <- lookupCommodity imb cy
  let qtSide = Quant.side . Quark.toQuant $ qk
  notSameSide qtSide s
  let q = Qty.fromCement . Pebble.toCement
        . Quark.toPebble . Quark.offset $ qk
  return $ Ent.T q cy a

toEnt imb (S s) a = do
  (cy, qk) <- oneCommodity imb
  notSameSide s (Quant.side . Quark.toQuant $ qk)
  let q = Qty.fromCement . Pebble.toCement
        . Quark.toPebble . Quark.offset $ qk
  return $ Ent.T q cy a

toEnt imb (UC phil cy _) a = do
  qk <- lookupCommodity imb cy
  let q = Qty.fromCement . Pebble.toCement
        . Philly.toPebble (Side.opposite . Quark.side $ qk)
        $ phil
  return $ Ent.T q cy a

toEnt imb (U phil) a = do
  (cy, qk) <- oneCommodity imb
  phillyIsSmallerAbsoluteValue phil qk
  let q = Qty.fromCement . Pebble.toCement
        . Philly.toPebble (Side.opposite . Quark.side $ qk)
        $ phil
  return $ Ent.T q cy a

toEnt imb (C cy) a = do
  qk <- lookupCommodity imb cy
  let q = Qty.fromCement . Pebble.toCement
        . Quark.toPebble . Quark.offset $ qk
  return $ Ent.T q cy a

toEnt imb E a = do
  (cy, qk) <- oneCommodity imb
  let q = Qty.fromCement . Pebble.toCement . Quark.toPebble
        . Quark.offset $ qk
  return $ Ent.T q cy a

phillyIsSmallerAbsoluteValue
  :: Philly.T
  -> Quark.T
  -> Either Error.T ()
phillyIsSmallerAbsoluteValue phil qk
  | Concrete.simpleCompare (abs . Qty.toConcrete $ p)
      (abs . Qty.toConcrete $ q) == LT = Right ()
  | otherwise = Left (Error.UnsignedTooLarge phil qk)
  where
    p = Qty.fromCement . Pebble.toCement . Philly.toPebble Side.Debit $ phil
    q = Qty.fromCement . Pebble.toCement . Quark.toPebble $ qk


notSameSide :: Side.T -> Side.T -> Either Error.T ()
notSameSide x y
  | x == y = Left $ Error.BalanceIsSameSide x
  | otherwise = return ()

oneCommodity :: Imbalances.T -> Either Error.T (Commodity.T, Quark.T)
oneCommodity imb = case M.toList . Imbalances.toMap $ imb of
  [] -> Left Error.NoImbalances
  (cy, q):[] -> return (cy, q)
  x:y:xs -> Left $ Error.MultipleImbalances x y xs

lookupCommodity :: Imbalances.T -> Commodity.T -> Either Error.T Quark.T
lookupCommodity imb cy = case M.lookup cy . Imbalances.toMap $ imb of
  Nothing -> Left $ Error.CommodityNotFound cy
  Just qk -> return qk
