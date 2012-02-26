-- | Options for the Postings report.
module Penny.Cabin.Postings.Options where

import qualified Data.Text as X
import Data.Time (formatTime)
import System.Locale (defaultTimeLocale)
import qualified Data.Time as Time
import System.Environment (getEnvironment)

import qualified Penny.Lincoln.Balance as Bal
import qualified Penny.Lincoln.Bits as Bits
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Cabin.Allocate as A
import qualified Penny.Cabin.Class as Cl
import qualified Penny.Cabin.Colors as CC
import qualified Penny.Cabin.Postings.Colors as C
import qualified Penny.Cabin.Postings.Fields as F
import qualified Penny.Cabin.Postings.Types as T
import qualified Penny.Cabin.Postings.Schemes.DarkBackground as Dark

data Options =
  Options { drCrColors :: C.DrCrColors
          , baseColors :: C.BaseColors 
          , dateFormat :: T.PostingInfo -> X.Text
          , qtyFormat :: T.PostingInfo -> X.Text
          , balanceFormat :: Bits.Commodity -> Bal.Nought -> X.Text
          , payeeAllocation :: A.Allocation
          , accountAllocation :: A.Allocation 
          , width :: Maybe Cl.ScreenWidth -> ReportWidth
          , subAccountLength :: Int
          , colorPref :: CC.ColorPref }

newtype ReportWidth = ReportWidth { unReportWidth :: Int }
                      deriving (Eq, Show, Ord)

ymd :: T.PostingInfo -> X.Text
ymd p = X.pack (formatTime defaultTimeLocale fmt d) where
  d = Time.utctDay . Bits.unDateTime . Q.dateTime . T.postingBox $ p
  fmt = "%Y-%m-%d"

qtyAsIs :: T.PostingInfo -> X.Text
qtyAsIs p = X.pack . show . Bits.unQty . Q.qty . T.postingBox $ p

balanceAsIs :: Bits.Commodity -> Bal.Nought -> X.Text
balanceAsIs _ n = case n of
  Bal.Zero -> X.pack "--"
  Bal.NonZero c -> X.pack . show . Bits.unQty . Bal.qty $ c

columnsVar :: IO (Maybe String)
columnsVar = getEnvironment >>= return . lookup "COLUMNS"
  
defaultWidth :: ReportWidth
defaultWidth = ReportWidth 80

columnsVarToWidth :: Maybe String -> ReportWidth
columnsVarToWidth ms = case ms of
  Nothing -> defaultWidth
  Just str -> case reads str of
    [] -> defaultWidth
    (i, []):[] -> if i > 0 then ReportWidth i else defaultWidth
    _ -> defaultWidth

useScreenWidth :: Maybe Cl.ScreenWidth -> ReportWidth
useScreenWidth sw = case sw of
  Nothing -> defaultWidth
  (Just (Cl.ScreenWidth w)) -> ReportWidth w

defaultOptions :: Options
defaultOptions =
  Options { drCrColors = Dark.drCrColors
          , baseColors = Dark.baseColors
          , dateFormat = ymd
          , qtyFormat = qtyAsIs
          , balanceFormat = balanceAsIs
          , payeeAllocation = A.allocation 40
          , accountAllocation = A.allocation 60
          , width = useScreenWidth
          , subAccountLength = 2
          , colorPref = CC.PrefAuto }

defaultFields :: F.Fields Bool
defaultFields =
  F.Fields { F.lineNum        = False
           , F.date           = True
           , F.flag           = False
           , F.number         = False
           , F.payee          = True
           , F.account        = True
           , F.postingDrCr    = True
           , F.postingCmdty   = True
           , F.postingQty     = True
           , F.totalDrCr      = True
           , F.totalCmdty     = True
           , F.totalQty       = True
           , F.tags           = False
           , F.memo           = False
           , F.filename       = False }

