module Penny.Trio where

import Penny.Common
import Penny.Numbers.Qty
import Penny.Numbers.Abstract.Aggregates
import Penny.Numbers.Abstract.RadGroup

-- | When building entries using the "Penny.Ents" module, you may
-- specify any of the 'Side', 'Commodity', a signed abstract quantity,
-- or an unsigned abstract quantity.  Which of these you specify,
-- combined with the running balance of the postings in the
-- 'Penny.Ents.Ents', determines whether an Ent is created or an error
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

data Trio
  = QC (Either (Polar Period Side) (Polar Comma Side))
       Commodity Arrangement
  -- ^ Specify a quantity, commodity, and how they are arranged, and a
  -- corresponding entry is always recorded.
  --
  -- Preconditions: none
  --
  -- Postconditions: the balance is appropriately affected.

  | Q (Either (Polar Period Side) (Polar Comma Side))
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

  | UC (Either (Unpolar Period) (Unpolar Comma)) Commodity Arrangement
  -- ^ Specify an unsigned abstract quantity only and a 'Commodity'
  -- and how they are arranged.
  --
  -- Preconditions: the imbalances contains the given commodity.
  --
  -- Postconditions: the given commodity in the imbalances either has
  -- its absolute value reduced or it flips to the opposite side.

  | U (Either (Unpolar Period) (Unpolar Comma))
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
