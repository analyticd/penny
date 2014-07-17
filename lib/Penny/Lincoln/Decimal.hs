{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- | Modules in this hierarchy deal with numbers.
--
-- There are two kinds of numbers.  Abstract representations have
-- type 'Abstract'.  You cannot do math on abstract representations.
-- However, they store the exact digit grouping representation and
-- radix point representation that was originally used to represent
-- the number.  In addition, only 'Abstract' can be rendered as a
-- string.
--
-- Concrete representations are 'Normal'.  They must be normal
-- numbers; no infinities or not-a-number values are allowed.
-- Internally they are signed decimal numbers.  These know nothing
-- about digit grouping or radix point representations.  You also
-- cannot render them as strings (there is a 'Show' instance, but it
-- is for debugging only.)  However, concrete types are the ones you must
-- use to perform arithmetic.  You can transform any 'Abstract' to a
-- concrete type, and the reverse is also true, although when you
-- transform a concrete type to an 'Abstract' you have to decide whether
-- you want digit grouping or not.
--
-- There are two types of concrete representations: 'Qty' and
-- 'Exchange'.  A 'Qty' is used in postings; an 'Exchange' in prices.
-- An 'Exchange' can be 'Pos' or "Neg' or zero, while a 'Qty' can be a
-- 'Debit' or 'Credit' or zero.  Not all zeroes are alike; the number
-- of decimal places is preserved.
--
-- The types of 'Abstract' numbers allow you to statically determine
-- whether you have a zero value or a non-zero value and an
-- accompanyin polarity.  The types of concrete numbers do not allow
-- this; you have to examine them at runtime to determine whether they
-- have polarity.
--
-- Both 'Concrete' and 'Abstract' store information not only about the
-- number itself (the quantity) but also about the 'Side'; that is,
-- whether the number is a 'Debit', 'Credit', or 'Center'.
--
-- Most types in this hierarchy are instances of 'Eq' and 'Ord'.
-- Most of these instances are derived.  They will not tell you
-- anything about how two numbers relate on the number line.
-- Exception: 'Concrete' has 'Eq' and 'Ord' instances based upon a
-- total ordering.  Larger numbers are greater than smaller numbers,
-- and (for example) @1.0000@ is less than @1.0@.
--
-- This module re-exports everything you need to work with 'Abstract',
-- 'Normal', 'Qty', and 'Exchange'.  If you need to manipulate
-- components of these types, you will have to import modules from
-- within the hierarchy.

module Penny.Lincoln.Decimal
  ( module Penny.Lincoln.Decimal.Abstract
  , module Penny.Lincoln.Decimal.Components
  , module Penny.Lincoln.Decimal.Concrete
  , module Penny.Lincoln.Decimal.Exchange
  , module Penny.Lincoln.Decimal.Frac
  , module Penny.Lincoln.Decimal.Groups
  , module Penny.Lincoln.Decimal.Laneless
  , module Penny.Lincoln.Decimal.Masuno
  , module Penny.Lincoln.Decimal.Normal
  , module Penny.Lincoln.Decimal.Render
  , module Penny.Lincoln.Decimal.Represent
  , module Penny.Lincoln.Decimal.Zero
  ) where

import Penny.Lincoln.Decimal.Abstract
import Penny.Lincoln.Decimal.Components
import Penny.Lincoln.Decimal.Concrete
import Penny.Lincoln.Decimal.Exchange
import Penny.Lincoln.Decimal.Frac
import Penny.Lincoln.Decimal.Groups
import Penny.Lincoln.Decimal.Laneless
import Penny.Lincoln.Decimal.Masuno
import Penny.Lincoln.Decimal.Normal
import Penny.Lincoln.Decimal.Render
import Penny.Lincoln.Decimal.Represent
import Penny.Lincoln.Decimal.Zero


