{- | Penny - double-entry accounting system

Penny is organized into a tree of modules, each with a name. Check out
the links for details on each component of Penny.

"Penny.Cabin" - Penny reports. Depends on Lincoln and Liberty.

"Penny.Copper" - the Penny parser. Depends on Lincoln.

"Penny.Liberty" - Penny command line parser helpers. Depends on
Lincoln and Copper.

"Penny.Lincoln" - the Penny core. Depends on no other Penny components.

"Penny.Shield" the Penny runtime environment

"Penny.Zinc" - the Penny command-line interface. Depends on Cabin,
Copper, Liberty, and Lincoln.

The dependencies are represented as a dot file in doc/dependencies.dot
in the Penny git repository.

This module exports a few functions that are useful for building a
simple command line program with default options. To make anything
more complicated you may need to import modules from deeper down
within the hierarchy, but for a program based on the defaults only you
should be able to import this module only (and nothing else).

-}
module Penny
  (  -- * Reports
    Cabin.allReportsWithDefaults

    -- * Parser defaults
  , Z.T(..)
  , Z.ColorToFile(..)
  , Z.defaultFromRuntime

    -- * Main function
  , defaultPenny
  , customPenny

    -- * Other useful stuff - for custom reports
  ) where

import qualified Penny.Cabin as Cabin
import qualified Penny.Shield as S
import qualified Penny.Zinc as Z

defaultPenny :: IO ()
defaultPenny = do
  rt <- S.runtime
  let df = Z.defaultFromRuntime
      rs = Cabin.allReportsWithDefaults
  Z.runPenny rt df rs

customPenny
  :: (S.Runtime -> Z.T)
  -> [Cabin.Report]
  -> IO ()
customPenny gd rs = do
  rt <- S.runtime
  Z.runPenny rt gd rs
