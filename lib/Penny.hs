{-# LANGUAGE ScopedTypeVariables #-}
-- | Main entry into the Penny REPL interface.
--
-- This interface is intended to be simple but as powerful as possible.

module Penny
  ( -- * Re-exported modules
    -- | All re-exported modules re-export all bindings from that module.
    module Penny.Balance
  , module Penny.BalanceCheck
  , module Penny.Clatch
  , module Penny.Colorize
  , module Penny.Command
  , module Penny.Commodity
  , module Penny.Copper
  , module Penny.Decimal
  , module Penny.Maypred
  , module Penny.NonEmpty
  , module Penny.NonZero
  , module Penny.Ord
  , module Penny.Polar
  , module Penny.Predicate
  , module Penny.Preset
  , module Penny.Qty
  , module Penny.Realm
  , module Penny.Report
  , module Penny.Scalar
  , module Penny.Scheme
  , module Penny.Serial
  , module Penny.Shortcut
  , module Penny.Stream
  , module Penny.Tree
  , module Penny.Troika

  , module Control.Lens.Getter
  , module Control.Lens.Setter
  , module Data.Monoid

  -- * Other re-exports
  , (&)
  , isPrefixOf
  , isSuffixOf
  , fromGregorian

  ) where

import Penny.Balance
import Penny.BalanceCheck
import Penny.Clatch
import Penny.Colorize
import Penny.Command
import Penny.Commodity
import Penny.Copper
import Penny.Decimal
import Penny.Maypred
import Penny.NonEmpty
import Penny.NonZero
import Penny.Ord
import Penny.Polar
import Penny.Predicate
import Penny.Preset
import Penny.Qty
import Penny.Realm
import Penny.Report
import Penny.Scalar
import Penny.Scheme
import Penny.Serial
import Penny.Shortcut
import Penny.Stream
import Penny.Tree
import Penny.Troika

import Control.Lens.Getter
import Control.Lens.Setter
import Data.Monoid
import Data.Function ((&))
import Data.Monoid.Cancellative (isPrefixOf, isSuffixOf)
import Data.Time (fromGregorian)
