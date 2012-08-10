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

data BalanceOpts = BalanceOpts {
  drCrColors :: C.DrCrColors
  , baseColors :: C.BaseColors
  , balanceFormat :: L.BottomLine -> X.Text
  , colorPref :: Chunk.Colors
  , showZeroBalances :: CO.ShowZeroBalances
  , defaultTimeZone :: Cop.DefaultTimeZone
  , convert :: Maybe (L.Commodity, L.DateTime)
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


balanceReport :: (S.Runtime -> BalanceOpts) -> I.Report
balanceReport f = I.Report H.help "balance" (parser f)


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

balanceAsIs :: L.BottomLine -> X.Text
balanceAsIs n = case n of
  L.Zero -> X.pack "--"
  L.NonZero c -> X.pack . show . L.unQty . Bal.qty $ c

