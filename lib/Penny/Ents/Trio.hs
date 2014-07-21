module Penny.Ents.Trio where

import Penny.Common
import Penny.Numbers.Qty

-- | When building entries using the "Penny.Ents.Ents" module, you may
-- specify any of the 'Side', 'Commodity', a signed abstract quantity,
-- or an unsigned abstract quantity.  Which of these you specify,
-- combined with the running balance of the postings in the
-- transaction, determines whether an Ent is created or an error
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
--
-- The first type parameter is a signed abstract type; the second type
-- parameter is an unsigned abstract type.

data Trio a b
  = QC a Commodity Arrangement
  -- ^ Specify a quantity, commodity, and how they are arranged, and a
  -- corresponding entry is always recorded.

  | Q a
  -- ^ Specify a quantity only.  If there is exactly one commodity in
  -- the balance, use it to infer the missing commodity.

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

  | UC b Commodity Arrangement
  -- ^ Specify an unsigned abstract quantity only and a 'Commodity'
  -- and how they are arranged.  If the given 'Commodity' is present
  -- in the balance, then offset the 'Commodity' by the unsigned
  -- amount given.  You must also specify how the unsigned quantity
  -- and the 'Commodity' are arranged.

  | U b
  -- ^ Specify an unsigned quantity only.  If there is one commodity
  -- in the balance, and the absolute value of its quantity is less
  -- than the absolute value of the unsigned quantity, then offset the
  -- commodity in the balance by the given unsigned quantity.

  | C Commodity
  -- ^ Specify a 'Commodity' only.  If the given commodity is in the
  -- balance, then offset it.

  | E
  -- ^ Specify nothing at all.  If there is one commodity in the
  -- balance, then offset it.

  deriving (Eq, Ord, Show)
