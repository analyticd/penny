-- | The Penny balance report
module Penny.Cabin.Balance (
  -- * Making reports
  
  -- | Use these functions if you are building a report from your own
  -- code and you are not using the Zinc parser.
  T.report
  , T.nullConvert
  , T.converter
    
    -- * Parsing reports
    -- | For use with the Zinc command line parser.
  , BalanceOpts(..)
  , balanceReport
  , defaultOptions
  , balanceAsIs

  ) where

import qualified Penny.Cabin.Balance.Tree as T
import qualified Penny.Cabin.Balance.Parser as P
import qualified Penny.Cabin.Balance.Help as H
import qualified Penny.Cabin.Chunk as Chunk
import qualified Penny.Cabin.Colors as C
import qualified Penny.Cabin.Colors.DarkBackground as Dark
import qualified Penny.Cabin.Interface as I
import qualified Penny.Cabin.Options as CO
import qualified Penny.Copper as Cop
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Balance as Bal
import qualified Penny.Shield as S

import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Text as X
import System.Console.MultiArg.Prim (Parser)

-- | Options for the balance report, when the options for the report
-- are being parsed in from a command line.
data BalanceOpts = BalanceOpts {
  drCrColors :: C.DrCrColors
  -- ^ Colors for debits and credits that are shown in the far right
  -- column of the report

  , baseColors :: C.BaseColors
    -- ^ Colors for all fields other than the debits and credits
    
  , balanceFormat :: L.BottomLine -> X.Text
    -- ^ Formats a BottomLine. For example you can use this function
    -- to deal with digit grouping.
    
  , colorPref :: Chunk.Colors
    -- ^ Show no colors, 8 colors, or 256 colors

  , showZeroBalances :: CO.ShowZeroBalances
    -- ^ Whether to completely omit balances that are zero, or show them

  , defaultTimeZone :: Cop.DefaultTimeZone
    -- ^ Time zone used when parsing dates and times from the command
    -- line. Has no impact on how values are formatted for display.

  , convert :: Maybe (L.Commodity, L.DateTime)
    -- ^ If Nothing, all entries are displayed in their original
    -- commodity. If Just, all entries are converted to the commodity
    -- given, based on the price at the given DateTime.
  }

toParseOpts :: BalanceOpts -> P.ParseOpts
toParseOpts b = P.ParseOpts {
  P.drCrColors = drCrColors b
  , P.baseColors = baseColors b
  , P.colorPref = colorPref b
  , P.showZeroBalances = showZeroBalances b
  , P.convert = convert b
  }

toTreeOpts :: P.ParseOpts -> BalanceOpts -> T.TreeOpts
toTreeOpts p b = T.TreeOpts {
  T.drCrColors = P.drCrColors p
  , T.baseColors = P.baseColors p
  , T.balanceFormat = balanceFormat b
  , T.showZeroBalances = P.showZeroBalances p
  }

parser :: (S.Runtime -> BalanceOpts) -> Parser I.ReportFunc
parser frt = do
  parsed <- P.parseOptions
  let rf rt _ _ ps pps = do
        let bo = frt rt
            po = toParseOpts bo
        po' <- Ex.mapException showParseErr $
               parsed rt (defaultTimeZone bo) po
        let to = toTreeOpts po' bo
        conv <- case P.convert po' of
          Nothing -> return $ T.nullConvert ps
          Just co -> T.converter co pps ps
        return
          . Chunk.chunksToText (P.colorPref po')
          . concat
          . T.report to
          $ conv
  return rf
        

showParseErr :: P.Error -> X.Text
showParseErr = X.pack . show


-- | Applied to a function that takes a Runtime and returns the
-- options for the Balance report, this function returns a Report that
-- conforms to the Cabin report interface.
balanceReport :: (S.Runtime -> BalanceOpts) -> I.Report
balanceReport f = I.Report H.help "balance" (parser f)


-- | Default options for the Balance report.
defaultOptions :: Cop.DefaultTimeZone -> S.Runtime -> BalanceOpts
defaultOptions dtz rt = BalanceOpts {
  drCrColors = Dark.drCrColors
  , baseColors = Dark.baseColors
  , balanceFormat = balanceAsIs
  , colorPref = CO.maxCapableColors rt
  , showZeroBalances = CO.ShowZeroBalances False
  , defaultTimeZone = dtz
  , convert = Nothing
  }

-- | Display a BottomLine, without any digit grouping.
balanceAsIs :: L.BottomLine -> X.Text
balanceAsIs n = case n of
  L.Zero -> X.pack "--"
  L.NonZero c -> X.pack . show . L.unQty . Bal.qty $ c

