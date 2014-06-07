-- | Provides a type to use when building entries.
module Penny.Lincoln.Trio where

import Penny.Lincoln.Decimal
import Penny.Lincoln.Common

-- | When building entries using the "Penny.Lincoln.Ents" module, you
-- must specify any of the 'Side', 'Commodity', or 'NZGrouped'.  You
-- can specify all three, or any of the three in any combination, or
-- none of the three.  If you do not specify all three,
-- "Penny.Lincoln.Ents" can, in some circumstances, infer the one you
-- did not specify.  "Penny.Lincoln.Ents" follows the rules specified
-- here.  If in any circumstance the specified conditions are not met,
-- "Penny.Lincoln.Ents" signals an error by returning a 'Left'.
--
-- The order of the postings is important because Penny keeps a
-- running balance of the postings it has seen so far.  If some
-- entries must be inferred, the inferred value depends on the balance
-- of the postings seen so far.
--
-- The naming scheme is cryptic: 'S' stands for @Side@, 'Z' for
-- @NZGrouped@, 'C' for @Commodity@, and 'E' for @Empty@.

data Trio
  = SZC Side NZGrouped Commodity Arrangement
  -- ^ Specify all three, and a corresponding entry is always
  -- recorded.  You must also specify how the commodity and the
  -- 'NZGrouped' are arranged.

  | SZ Side NZGrouped
  -- ^ Specify the 'Side' and the 'NZGrouped'.  If there is exactly
  -- one commodity in the balance, use it to infer the missing
  -- commodity.

  | SC Side Commodity
  -- ^ Specify the 'Side' and the 'Commodity'.  If there is a
  -- commodity in the balance, and the commodity in the balance has a
  -- side that is opposite the side specified here, then record an
  -- entry that offsets the commodity in the balance.

  | S Side
  -- ^ Specify a 'Side' only.  If there is one commodity in the
  -- balance, and that commodity has a 'Side' opposite to the one
  -- specified here, then record an entry that offests the commodity
  -- in the balance.

  | ZC NZGrouped Commodity Arrangement
  -- ^ Specify a 'NZGrouped' and a 'Commodity'.  If the given
  -- 'Commodity' is present in the balance, then offset the
  -- 'Commodity' by the 'NZGrouped' given.  You must also specify how
  -- the 'NZGrouped' and the 'Commodity' are arranged.

  | Z NZGrouped
  -- ^ Specify the 'NZGrouped' only.  If there is one commodity in the
  -- balance, and the absolute value of its quantity is less than the
  -- absolute value of the 'NZGrouped', then offset the commodity in
  -- the balance by the given 'NZGrouped'.

  | C Commodity
  -- ^ Specify a 'Commodity' only.  If the given commodity is in the
  -- balance, then offset it.

  | E
  -- ^ Specify nothing at all.  If there is one commodity in the
  -- balance, then offset it.

  deriving (Eq, Ord, Show)
