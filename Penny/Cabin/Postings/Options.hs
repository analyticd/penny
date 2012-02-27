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
import qualified Penny.Cabin.Colors as CC
import qualified Penny.Cabin.Postings.Colors as C
import qualified Penny.Cabin.Postings.Fields as F
import qualified Penny.Cabin.Postings.Types as T
import qualified Penny.Cabin.Postings.Schemes.DarkBackground as Dark
import qualified Penny.Cabin.Types as CT
import Penny.Copper.DateTime (DefaultTimeZone)
import Penny.Copper.Qty (Radix, Separator)


data Options =
  Options { drCrColors :: C.DrCrColors
            -- ^ Colors to use when displaying debits, credits, and
            -- when displaying balance totals

          , baseColors :: C.BaseColors 
            -- ^ Colors to use when displaying everything else

          , dateFormat :: T.PostingInfo -> X.Text
            -- ^ How to display dates. This function is applied to the
            -- a PostingInfo so it has lots of information, but it
            -- should return a date for use in the Date field.
            
          , qtyFormat :: T.PostingInfo -> X.Text
            -- ^ How to display the quantity of the posting. This
            -- function is applied to a PostingInfo so it has lots of
            -- information, but it should return a formatted string of
            -- the quantity. Allows you to format digit grouping,
            -- radix points, perform rounding, etc.
            
          , balanceFormat :: Bits.Commodity -> Bal.Nought -> X.Text
            -- ^ How to display balance totals. Similar to
            -- balanceFormat.
            
          , payeeAllocation :: A.Allocation
            -- ^ This and accountAllocation determine how much space
            -- payees and accounts receive. They divide up the
            -- remaining space after everything else is displayed. For
            -- instance if payeeAllocation is 60 and accountAllocation
            -- is 40, the payee takes about 60 percent of the
            -- remaining space and the account takes about 40 percent.
            
          , accountAllocation :: A.Allocation 
            -- ^ See payeeAllocation above

          , width :: ReportWidth
            -- ^ Gives the default report width. This can be
            -- overridden on the command line. You can use the
            -- information from the Runtime to make this as wide as
            -- the as the current terminal.

          , subAccountLength :: Int
            -- ^ When shortening the names of sub accounts to make
            -- them fit, they will be this long.

          , colorPref :: CC.ColorPref
            -- ^ How many colors you want to see, or do it
            -- automatically.

          , timeZone :: DefaultTimeZone
            -- ^ When dates and times are given on the command line
            -- and they have no time zone, they are assumed to be in
            -- this time zone. This has no bearing on how dates are
            -- formatted in the output; for that, see dateFormat
            -- above.

          , radix :: Radix
            -- ^ The character used for the radix point for numbers
            -- given on the command line (e.g. a full stop, or a
            -- comma). Affects how inputs are parsed. Has no bearing
            -- on how output is formatted; for that, see qtyFormat and
            -- balanceFormat above.
            
          , separator :: Separator
            -- ^ The character used as the digit group separator for
            -- numbers parsed from the command line (e.g. a full stop,
            -- or a comma). Affects how inputs are parsed. Has no
            -- bearing on how output is formatted; for that, see
            -- qtyFormat and balanceFormat above.
          }

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

defaultOptions ::
  DefaultTimeZone
  -> Radix
  -> Separator
  -> CT.Runtime
  -> Options
defaultOptions dtz rad sep rt =
  Options { drCrColors = Dark.drCrColors
          , baseColors = Dark.baseColors
          , dateFormat = ymd
          , qtyFormat = qtyAsIs
          , balanceFormat = balanceAsIs
          , payeeAllocation = A.allocation 40
          , accountAllocation = A.allocation 60
          , width = widthFromRuntime rt
          , subAccountLength = 2
          , colorPref = CC.PrefAuto 
          , timeZone = dtz
          , radix = rad
          , separator = sep }

widthFromRuntime :: CT.Runtime -> ReportWidth
widthFromRuntime rt = case CT.screenWidth rt of
  Nothing -> defaultWidth
  Just (CT.ScreenWidth w) -> ReportWidth w

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

