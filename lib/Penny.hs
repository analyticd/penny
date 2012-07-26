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
module Penny (
  -- * Default time zone
  Copper.DefaultTimeZone (DefaultTimeZone)
  , Copper.utcDefault
  , minsToDefaultTimeZone
    
    -- * Radix and grouping characters
  , Copper.RadGroup
  , Copper.periodComma
  , Copper.periodSpace
  , Copper.commaPeriod
  , Copper.commaSpace
    
    -- * Reports
  , Cabin.allReportsWithDefaults
    
    -- * Main function
  , Z.runPenny
  , defaultPenny
    
    -- * Other useful stuff - for custom reports
  ) where

import qualified Penny.Cabin as Cabin
import qualified Penny.Copper as Copper
import qualified Penny.Lincoln as L
import qualified Penny.Shield as S
import qualified Penny.Zinc as Z

-- | Use to make a DefaultTimeZone based on the number of minutes the
-- time zone is offset from UTC. Make sure the argument you supply is
-- between (-840) and 840; otherwise your program will crash at
-- runtime.
minsToDefaultTimeZone :: Int -> Copper.DefaultTimeZone
minsToDefaultTimeZone i = case L.minsToOffset i of
  Nothing -> error $ "penny: error: minutes out of range of "
             ++ "allowed values for time zone"
  Just tzo -> Copper.DefaultTimeZone tzo

defaultPenny :: Copper.DefaultTimeZone -> Copper.RadGroup -> IO ()
defaultPenny dtz rg = do
  rt <- S.runtime
  let df = Z.defaultFromRuntime dtz rg
      rs = Cabin.allReportsWithDefaults dtz rg
  Z.runPenny rt dtz rg df rs
