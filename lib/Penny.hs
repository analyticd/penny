{-# LANGUAGE ScopedTypeVariables #-}
-- | Main entry into the Penny REPL interface.
--
-- This interface is intended to be simple but as powerful as possible.

module Penny
  ( -- * Re-exported modules
    -- | All re-exported modules re-export all bindings from that module.
    module Penny.Balance
  , module Penny.Clatch
  , module Penny.Colorize
  , module Penny.Command
  , module Penny.Commodity
  , module Penny.Decimal
  , module Penny.Natural
  , module Penny.NonEmpty
  , module Penny.NonZero
  , module Penny.Polar
  , module Penny.Qty
  , module Penny.Realm
  , module Penny.Report
  , module Penny.Scalar
  , module Penny.Serial
  , module Penny.Shortcut
  , module Penny.Stream
  , module Penny.Tree
  , module Penny.Troika

  -- * Other re-exports

  -- | These bindings are re-exported from particular modules.  The
  -- entire module from which they come is not re-exported.

  , Penny.Colors.Colors
  , Penny.Colors.dark
  , Penny.Colors.light
  , Penny.Columns.Colable
  , Penny.Columns.Columns
  , Penny.Clatcher.Clatcher
  , Penny.Clatcher.clatcher
  , (<>)

  ) where

import Penny.Balance
import Penny.Clatch
import Penny.Clatcher (Clatcher, clatcher)
import Penny.Colorize
import Penny.Colors (Colors, dark, light)
import Penny.Columns (Colable, Columns)
import Penny.Command
import Penny.Commodity
import Penny.Decimal
import Penny.Natural
import Penny.NonEmpty
import Penny.NonZero
import Penny.Polar
import Penny.Qty
import Penny.Realm
import Penny.Report
import Penny.Scalar
import Penny.Serial
import Penny.Shortcut
import Penny.Stream
import Penny.Tree
import Penny.Troika

import Data.Monoid ((<>))
