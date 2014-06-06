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
-- Concrete representations have type 'Concrete'.  These know
-- nothing about digit grouping or radix point representations.  You
-- also cannot render them as strings (there is a 'Show' instance,
-- but it is for debugging only.)  However, 'Concrete' is the type
-- you must use to perform arithmetic.  You can transform any
-- 'Abstract' to a 'Concrete', and the reverse is also true,
-- although when you transform a 'Concrete' to an 'Abstract'  you
-- have to decide whether you want digit grouping or not.
--
-- Both 'Concrete' and 'Abstract' store information not only about the
-- number itself (the quantity) but also about the 'Side'; that is,
-- whether the number is a 'Debit', 'Credit', or 'Center'.
--
-- A single type, 'Amount', can hold either a 'Amount' or a 'Concrete'.
--
-- Most types in this hierarchy are instances of 'Eq' and 'Ord'.
-- Most of these instances are derived.  They will not tell you
-- anything about how two numbers relate on the number line.
-- Exception: 'Concrete' has 'Eq' and 'Ord' instances based upon a
-- total ordering.  Larger numbers are greater than smaller numbers,
-- and (for example) @1.0000@ is less than @1.0@.
--
-- This module re-exports everything you need to work with 'Amount',
-- 'Abstract', and 'Concrete'.  If you need to manipulate components of
-- these types, you will have to import modules from within the
-- hierarchy.

module Penny.Lincoln.Decimal
  ( -- * Basic components
    Side(..)
  , PosNeg(..)
  , Opposite(..)
  , Lane(..)

  -- * Abstract
  , Rep
  , Abstract

  -- * Concrete
  , Normal(..)
  , Qty
  , Exchange
  , negate

  -- * Conversions
  , RadGroup(..)
  , grouped
  , ungrouped
  , exchange

  -- * Rendering
  , Renderable(..)

  -- * Amount
  , Amount(..)

  ) where

import Penny.Lincoln.Decimal.Components hiding (Abstract)
import Penny.Lincoln.Decimal.Represent
import Penny.Lincoln.Decimal.Concrete
import Penny.Lincoln.Decimal.Abstract
import Penny.Lincoln.Decimal.Amount
import Penny.Lincoln.Decimal.Render
import Penny.Lincoln.Decimal.Normal
import Penny.Lincoln.Decimal.Exchange
import Deka.Dec (PosNeg(..))
import Prelude hiding (negate)
