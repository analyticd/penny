module Penny.Rep where

import Penny.Polar
import Penny.Grammar

-- # Qty representations

-- | Qty representations that may be neutral or non-neutral.  The type
-- variable is the type of the radix point and grouping character;
-- see, for example, 'RadCom' or 'RadPer'.  If non-neutral, also
-- contains a 'Side'.
--
-- This is a complete representation of a quantity; that is, it can
-- represent any quantity.

type Rep r = Moderated (Nil r) (Brim r)

-- | Qty representations that may be neutral or non-neutral and have a
-- radix that is either a period or a comma.  If non-neutral, also
-- contains a 'Side'.

type RepAnyRadix = Either (Rep RadCom) (Rep RadPer)


-- # Conversions

-- | Removes the 'Side' from a 'QtyRepAnyRadix'.
c'NilOrBrimScalarAnyRadix'RepAnyRadix
  :: RepAnyRadix
  -> NilOrBrimScalarAnyRadix
c'NilOrBrimScalarAnyRadix'RepAnyRadix ei = case ei of
  Left rc -> Left $ case rc of
    Moderate n -> Left n
    Extreme (Polarized c _) -> Right c
  Right rp -> Right $ case rp of
    Moderate n -> Left n
    Extreme (Polarized c _) -> Right c
