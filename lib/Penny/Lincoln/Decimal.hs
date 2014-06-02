{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- | Modules in this hierarchy deal with numbers.
--
-- There are two kinds of numbers.  Abstract representations have
-- type 'Rep'.  You cannot do math on abstract representations.
-- However, they can store the exact digit grouping representation
-- that was originally used to represent the number.
--
-- Concrete representations have type 'Concrete'.  These know
-- nothing about digit grouping or radix points.  They are the only
-- way you can do arithmetic.  You can transform any 'Rep' to a
-- 'Concrete', and the reverse is also true, although when you
-- transform a 'Concrete' to a 'Rep'  you have to decide whether you
-- want digit grouping or not.
--
-- Both 'Concrete' and 'Rep' store information not only about the
-- number itself (the quantity) but also about the 'Side'; that is,
-- whether the number is a 'Debit', 'Credit', or 'Center'.
--
-- A single type, 'Amount', can hold either a 'Rep' or a 'Concrete'.

module Penny.Lincoln.Decimal where

import Penny.Lincoln.Decimal.Concrete
import Penny.Lincoln.Decimal.Rep
import Penny.Lincoln.Decimal.Side
import Penny.Lincoln.Decimal.Amount
import Penny.Lincoln.Decimal.Lane
