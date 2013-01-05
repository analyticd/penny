{- | Penny - double-entry accounting system

Penny is organized into a tree of modules, each with a name. Check out
the links for details on each component of Penny.

"Penny.Brenner" - Penny financial institution transaction
handling. Depends on Lincoln and Copper.

"Penny.Cabin" - Penny reports. Depends on Lincoln and Liberty.

"Penny.Copper" - the Penny parser. Depends on Lincoln.

"Penny.Liberty" - Penny command line parser helpers. Depends on
Lincoln and Copper.

"Penny.Lincoln" - the Penny core. Depends on no other Penny components.

"Penny.Shield" the Penny runtime environment. Depends on Lincoln.

"Penny.Zinc" - the Penny command-line interface. Depends on Cabin,
Copper, Liberty, and Lincoln.

The dependencies are represented as a dot file in bin/doc/dependencies.dot
in the Penny git repository.

This module exports a few functions that are useful for building a
simple command line program with default options. To make anything
more complicated you may need to import modules from deeper down
within the hierarchy, but for a program based on the defaults only you
should be able to import this module only (and nothing else).

-}
module Penny where

import qualified Data.Text as X
import qualified Penny.Cabin.Balance.Convert.Parser as CP
import qualified Penny.Cabin.Posts.Fields as PF
import qualified Penny.Cabin.Posts.Spacers as PS
import qualified Penny.Cabin.Posts.Meta as M
import qualified Penny.Cabin.Scheme as E
import qualified Penny.Lincoln as L
import qualified Penny.Zinc as Z
import qualified Penny.Shield as S

data Defaults = Defaults
  { caseSensitive :: Bool
  , matcher :: L.Matcher
  , colorToFile :: Bool
  , defaultScheme :: Maybe E.Scheme
  , additionalSchemes :: [E.Scheme]
  , sorter :: [(Z.SortField, Z.Direction)]

  , balanceFormat :: L.Commodity -> L.Qty -> X.Text
  , balanceShowZeroBalances :: Bool
  , balanceOrder :: L.SubAccount -> L.SubAccount -> Ordering

  , convertShowZeroBalances :: Bool
  , convertTarget :: CP.Target
  , convertDateTime :: L.DateTime
  , convertOrder :: CP.SortOrder
  , convertSortBy :: CP.SortBy
  , convertFormat :: L.Qty -> X.Text

  , postingsFields :: PF.Fields Bool
  , postingsWidth :: Int
  , postingsShowZeroBalances :: Bool
  , postingsDateFormat :: M.Box -> X.Text
  , postingsQtyFormat :: M.Box -> X.Text
  , postingsBalanceFormat :: L.Commodity -> L.Qty -> X.Text
  , postingsSubAccountLength :: Int
  , postingsPayeeAllocation :: Int
  , postingsAccountAllocation :: Int
  , postingsSpacers :: PS.Spacers Int
  }

runPenny :: (S.Runtime -> Defaults) -> IO ()
runPenny = undefined
