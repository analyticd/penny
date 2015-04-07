-- | Prices convert quantities from one commodity to another.
--
-- A price consists of:
--
-- * a commodity to convert from, a 'FromCy'
--
-- * a commodity to convert to, a 'ToCy'
--
-- * a specification of the ratio of 'FromCy' to 'ToCy', an 'Exch'
--
-- * an instant in time when this price took effect, a
-- 'Penny.DateTime.DateTime'
--
-- Typically the 'ToCy' is the home currency, while the 'FromCy' is an
-- asset of some sort, such as stock shares.  The 'Exch' always means
-- that 1 unit of the 'FromCy' equals this many units of the 'ToCy'.
-- If the 'Exch' is 'Penny.PluMin.Plus', then the price is
-- positive; otherwise, the price is negative.  For example, let's say
-- that one share of Ford Motor, which is @F@, is worth @$15.50@.
-- Here, the 'FromCy' is @F@, the 'ToCy' is @$@, and the 'Exch' is
-- @15.50@.
--
-- Typically the 'Exch' will be positive (which is represented in a
-- 'Penny.Number.Rep.ExchRep' with a 'Penny.PluMin.Plus'),
-- but a negative 'Exch' might be useful to represent assets that have
-- negative value (perhaps toxic waste?) though you could also book
-- these as liabilities instead.

module Penny.Prices
  ( FromCy(..)
  , ToCy(..)
  , FromTo
  , fromCy
  , toCy
  , fromTo
  , convertQty
  , Price(..)
  , PriceDb
  , emptyDb
  , addPriceToDb
  , lookupExch
  , ExchLookupError(..)
  ) where

import Penny.Prices.Internal
