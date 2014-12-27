module Penny.Lincoln.Trio where

import Penny.Lincoln.Commodity
import Penny.Lincoln.Qty

data Trio
  = QC Qty Commodity
  -- ^ Specify a quantity and commodity and a corresponding entry is
  -- always recorded.
  --
  -- Preconditions: none
  --
  -- Postconditions: the balance is appropriately affected.

  | Q Qty
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

  | UC QtyUnsigned Commodity
  -- ^ Specify an unsigned abstract quantity only and a 'Commodity'
  -- and how they are arranged.
  --
  -- Preconditions: the imbalances contains the given commodity.
  --
  -- Postconditions: the given commodity in the imbalances either has
  -- its absolute value reduced or it flips to the opposite side.


  | U QtyUnsigned
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

