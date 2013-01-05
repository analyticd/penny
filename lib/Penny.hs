-- | Penny - extensible double-entry accounting system

module Penny
  ( -- * Building a custom Penny binary

    -- | Everything you need to create a custom Penny program is
    -- available by importing only this module.
    Defaults(..)
  , Z.Matcher(..)

  -- ** Color schemes
  , E.Scheme(..)
  , E.TextSpecs
  , E.Labels(..)
  , E.EvenAndOdd(..)
  , module Penny.Cabin.Chunk

  -- ** Sorting
  , Z.SortField(..)
  , Z.Direction(..)

  -- ** Main function
  , runPenny

    -- * Developer overview

    -- | Penny is organized into a tree of modules, each with a
    -- name. Check out the links for details on each component of
    -- Penny.
    --
    -- "Penny.Brenner" - Penny financial institution transaction
    -- handling. Depends on Lincoln and Copper.
    --
    -- "Penny.Cabin" - Penny reports. Depends on Lincoln and Liberty.
    --
    -- "Penny.Copper" - the Penny parser. Depends on Lincoln.
    --
    -- "Penny.Liberty" - Penny command line parser helpers. Depends on
    -- Lincoln and Copper.
    --
    -- "Penny.Lincoln" - the Penny core. Depends on no other Penny
    -- components.
    --
    -- "Penny.Shield" the Penny runtime environment. Depends on
    -- Lincoln.
    --
    -- "Penny.Zinc" - the Penny command-line interface. Depends on
    -- Cabin, Copper, Liberty, and Lincoln.
    --
    -- The dependencies are represented as a dot file in
    -- bin/doc/dependencies.dot in the Penny git repository.
  ) where

import qualified Data.Text as X
import qualified Penny.Cabin.Balance.Convert as Conv
import qualified Penny.Cabin.Balance.Convert.Parser as CP
import qualified Penny.Cabin.Balance.Convert.Options as ConvOpts
import qualified Penny.Cabin.Balance.MultiCommodity as MC
import qualified Penny.Cabin.Balance.MultiCommodity.Parser as MP
import Penny.Cabin.Chunk
import qualified Penny.Cabin.Interface as I
import qualified Penny.Cabin.Options as CO
import qualified Penny.Cabin.Parsers as CabP
import qualified Penny.Cabin.Posts as Ps
import qualified Penny.Cabin.Posts.Fields as PF
import qualified Penny.Cabin.Posts.Spacers as PS
import qualified Penny.Cabin.Posts.Meta as M
import qualified Penny.Cabin.Scheme as E
import qualified Penny.Lincoln as L
import qualified Penny.Zinc as Z
import qualified Penny.Shield as S
import qualified Text.Matchers.Text as Mr

-- | This type contains settings for all the reports, as well as
-- default settings for the global options. Some of these can be
-- overridden on the command line.
data Defaults = Defaults
  { caseSensitive :: Bool
  , matcher :: Z.Matcher
  , colorToFile :: Bool
  , defaultScheme :: Maybe E.Scheme
  , additionalSchemes :: [E.Scheme]
  , sorter :: [(Z.SortField, Z.Direction)]

  , balanceFormat :: L.Commodity -> L.Qty -> X.Text
  , balanceShowZeroBalances :: Bool
  , balanceOrder :: CabP.SortOrder

  , convertShowZeroBalances :: Bool
  , convertTarget :: CP.Target
  , convertOrder :: CabP.SortOrder
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
runPenny getDefaults = do
  rt <- S.runtime
  let df = getDefaults rt
      rs = allReports df
  Z.runZinc (toZincDefaults df) rt rs

allReports
  :: Defaults
  -> [I.Report]
allReports df =
  let bd = toBalanceDefaults df
      cd = toConvertDefaults df
      pd = toPostingsDefaults df
  in [ Ps.zincReport pd
     , MC.parseReport (balanceFormat df) bd
     , Conv.cmdLineReport cd
     ]

toZincDefaults :: Defaults -> Z.Defaults
toZincDefaults d = Z.Defaults
  { Z.sensitive =
      if caseSensitive d then Mr.Sensitive else Mr.Insensitive
  , Z.matcher = matcher d
  , Z.colorToFile = Z.ColorToFile . colorToFile $ d
  , Z.defaultScheme = defaultScheme d
  , Z.moreSchemes = additionalSchemes d
  , Z.sorter = sorter d
  }

toBalanceDefaults :: Defaults -> MP.ParseOpts
toBalanceDefaults d = MP.ParseOpts
  { MP.showZeroBalances =
      CO.ShowZeroBalances . balanceShowZeroBalances $ d
  , MP.order = balanceOrder d
  , MP.needsHelp = False
  }

toConvertDefaults :: Defaults -> ConvOpts.DefaultOpts
toConvertDefaults d = ConvOpts.DefaultOpts
  { ConvOpts.showZeroBalances =
      CO.ShowZeroBalances . convertShowZeroBalances $ d
  , ConvOpts.target = convertTarget d
  , ConvOpts.sortOrder = convertOrder d
  , ConvOpts.sortBy = convertSortBy d
  , ConvOpts.format = convertFormat d
  }

toPostingsDefaults :: Defaults -> Ps.ZincOpts
toPostingsDefaults d = Ps.ZincOpts
  { Ps.fields = postingsFields d
  , Ps.width = Ps.ReportWidth . postingsWidth $ d
  , Ps.showZeroBalances =
      CO.ShowZeroBalances . postingsShowZeroBalances $ d
  , Ps.dateFormat = postingsDateFormat d
  , Ps.qtyFormat = postingsQtyFormat d
  , Ps.balanceFormat = postingsBalanceFormat d
  , Ps.subAccountLength =
      Ps.SubAccountLength . postingsSubAccountLength $ d
  , Ps.payeeAllocation =
      Ps.alloc . postingsPayeeAllocation $ d
  , Ps.accountAllocation =
      Ps.alloc . postingsAccountAllocation $ d
  , Ps.spacers = postingsSpacers d
  }
