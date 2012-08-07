-- | Options when using the Posts report with the Zinc command line
-- interface. Also has some functions that are useful as defaults.
module Penny.Cabin.Posts.ZincOpts where

import qualified Penny.Copper as Cop
import qualified Penny.Cabin.Chunk as CC
import qualified Penny.Cabin.Colors as C
import qualified Penny.Cabin.Colors.DarkBackground as Dark
import qualified Penny.Cabin.Options as O
import qualified Penny.Cabin.Posts.Allocate as A
import qualified Penny.Cabin.Posts.Fields as F
import qualified Penny.Cabin.Posts.Options as PO
import qualified Penny.Cabin.Posts.Spacers as S
import Penny.Cabin.Posts.Meta (Box)
import qualified Penny.Lincoln.Balance as Bal
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Shield as Sh

import qualified Data.Time as T
import qualified Data.Text as X
import System.Locale (defaultTimeLocale)
import qualified Text.Matchers.Text as M

-- | Provides a type that is used to configure the compile-time
-- defaults for the Posts report. Some of the aspects of the Postings
-- report that can be configured in the Options module make no sense
-- to be included in this type, as they will always be overriden by
-- the command line (for example, the default matcher will always
-- depend on what has already been seen on the command line.) Thus
-- this type includes only the aspects of the Postings report that can
-- be configured at compile type when being used with Zinc.
data ZincOpts =
  ZincOpts {
    drCrColors :: C.DrCrColors
    -- ^ Colors to use when displaying debits, credits, and
    -- when displaying balance totals

    , baseColors :: C.BaseColors 
      -- ^ Colors to use when displaying everything else

    , dateFormat :: Box -> X.Text
      -- ^ How to display dates. This function is applied to the
      -- a PostingInfo so it has lots of information, but it
      -- should return a date for use in the Date field.
      
    , qtyFormat :: Box -> X.Text
      -- ^ How to display the quantity of the posting. This
      -- function is applied to a Box so it has lots of
      -- information, but it should return a formatted string of
      -- the quantity. Allows you to format digit grouping,
      -- radix points, perform rounding, etc.
      
    , balanceFormat :: L.Commodity -> L.BottomLine -> X.Text
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

    , width :: PO.ReportWidth
      -- ^ Gives the default report width. This can be
      -- overridden on the command line. You can use the
      -- information from the Runtime to make this as wide as
      -- the current terminal.

    , subAccountLength :: Int
      -- ^ When shortening the names of sub accounts to make
      -- them fit, they will be this long.

    , colorPref :: CC.Colors
      -- ^ How many colors you want to see, or do it
      -- automatically.

    , timeZone :: Cop.DefaultTimeZone
      -- ^ When dates and times are given on the command line
      -- and they have no time zone, they are assumed to be in
      -- this time zone. This has no bearing on how dates are
      -- formatted in the output; for that, see dateFormat
      -- above.

    , radGroup :: Cop.RadGroup
      -- ^ The characters used for the radix point for numbers
      -- given on the command line (e.g. a full stop, or a
      -- comma) and for the digit group separator for
      -- numbers parsed from the command line (e.g. a full stop,
      -- or a comma). Affects how inputs are parsed. Has no bearing
      -- on how output is formatted; for that, see qtyFormat and
      -- balanceFormat above.
      
    , fields :: F.Fields Bool
      -- ^ Default fields to show in the report.
      
    , spacers :: S.Spacers Int
      -- ^ Default width for spacer fields. If any of these Ints are
      -- less than or equal to zero, there will be no spacer. There is
      -- never a spacer for fields that do not appear in the report.
      
    , showZeroBalances :: O.ShowZeroBalances
      -- ^ Are commodities that have no balance shown in the Total fields
      -- of the report?
    }


-- | Convert a ZincOpts to an Options.
toOptions :: ZincOpts -> PO.Options
toOptions z = PO.Options {
  PO.drCrColors = drCrColors z
  , PO.baseColors = baseColors z
  , PO.dateFormat = dateFormat z
  , PO.qtyFormat = qtyFormat z
  , PO.balanceFormat = balanceFormat z
  , PO.payeeAllocation = payeeAllocation z
  , PO.accountAllocation = accountAllocation z
  , PO.width = width z
  , PO.subAccountLength = subAccountLength z
  , PO.colorPref = colorPref z
  , PO.timeZone = timeZone z
  , PO.radGroup = radGroup z
  , PO.sensitive = M.Insensitive
  , PO.factory = \c t -> return (M.within c t)
  , PO.tokens = []
  , PO.postFilter = []
  , PO.fields = defaultFields
  , PO.spacers = defaultSpacerWidth
  , PO.showZeroBalances = showZeroBalances z }

-- | Shows the date of a posting in YYYY-MM-DD format.
ymd :: Box -> X.Text
ymd p = X.pack (T.formatTime defaultTimeLocale fmt d) where
  d = T.localDay
      . L.localTime
      . Q.dateTime
      . L.boxPostFam
      $ p
  fmt = "%Y-%m-%d"

-- | Shows the quantity of a posting. Does no rounding or
-- prettification; simply uses show on the underlying Decimal.
qtyAsIs :: Box -> X.Text
qtyAsIs p = X.pack . show . L.unQty . Q.qty . L.boxPostFam $ p

-- | Shows the quantity of a balance. If there is no quantity, shows
-- two dashes.
balanceAsIs :: a -> L.BottomLine -> X.Text
balanceAsIs _ n = case n of
  L.Zero -> X.pack "--"
  L.NonZero c -> X.pack . show . L.unQty . Bal.qty $ c

-- | The default width for the report.
defaultWidth :: PO.ReportWidth
defaultWidth = PO.ReportWidth 80

-- | Applied to the value of the COLUMNS environment variable, returns
-- an appropriate ReportWidth.
columnsVarToWidth :: Maybe String -> PO.ReportWidth
columnsVarToWidth ms = case ms of
  Nothing -> defaultWidth
  Just str -> case reads str of
    [] -> defaultWidth
    (i, []):[] -> if i > 0 then PO.ReportWidth i else defaultWidth
    _ -> defaultWidth

-- | Obtains a default Options.
defaultOptions ::
  Cop.DefaultTimeZone
  -> Cop.RadGroup
  -> Sh.Runtime
  -> ZincOpts
defaultOptions dtz rg rt =
  ZincOpts { drCrColors = Dark.drCrColors
    , baseColors = Dark.baseColors
    , dateFormat = ymd
    , qtyFormat = qtyAsIs
    , balanceFormat = balanceAsIs
    , payeeAllocation = A.allocation 40
    , accountAllocation = A.allocation 60
    , width = widthFromRuntime rt
    , subAccountLength = 2
    , colorPref = O.autoColors O.PrefAuto rt
    , timeZone = dtz
    , radGroup = rg
    , fields = defaultFields
    , spacers = defaultSpacerWidth
    , showZeroBalances = O.ShowZeroBalances False }

-- | Given the Runtime, use the defaultWidth given above to calculate
-- the report's width if COLUMNS does not yield a value. Otherwise,
-- use what is in COLUMNS.
widthFromRuntime :: Sh.Runtime -> PO.ReportWidth
widthFromRuntime rt = case Sh.screenWidth rt of
  Nothing -> defaultWidth
  Just w -> PO.ReportWidth . Sh.unScreenWidth $ w

-- | Default fields to show in the Postings report.
defaultFields :: F.Fields Bool
defaultFields =
  F.Fields { F.globalTransaction    = False
           , F.revGlobalTransaction = False
           , F.globalPosting        = False
           , F.revGlobalPosting     = False
           , F.fileTransaction      = False
           , F.revFileTransaction   = False
           , F.filePosting          = False
           , F.revFilePosting       = False
           , F.filtered             = False
           , F.revFiltered          = False
           , F.sorted               = False
           , F.revSorted            = False
           , F.visible              = False
           , F.revVisible           = False
           , F.lineNum              = False
           , F.date                 = True
           , F.flag                 = False
           , F.number               = False
           , F.payee                = True
           , F.account              = True
           , F.postingDrCr          = True
           , F.postingCmdty         = True
           , F.postingQty           = True
           , F.totalDrCr            = True
           , F.totalCmdty           = True
           , F.totalQty             = True
           , F.tags                 = False
           , F.memo                 = False
           , F.filename             = False }

-- | Default width of spacers; most are one character wide, but the
-- spacer after payee is 4 characters wide.
defaultSpacerWidth :: S.Spacers Int
defaultSpacerWidth =
  S.Spacers { S.globalTransaction    = 1
            , S.revGlobalTransaction = 1
            , S.globalPosting        = 1
            , S.revGlobalPosting     = 1
            , S.fileTransaction      = 1
            , S.revFileTransaction   = 1
            , S.filePosting          = 1
            , S.revFilePosting       = 1
            , S.filtered             = 1
            , S.revFiltered          = 1
            , S.sorted               = 1
            , S.revSorted            = 1
            , S.visible              = 1
            , S.revVisible           = 1
            , S.lineNum              = 1
            , S.date                 = 1
            , S.flag                 = 1
            , S.number               = 1
            , S.payee                = 4
            , S.account              = 1
            , S.postingDrCr          = 1
            , S.postingCmdty         = 1
            , S.postingQty           = 1
            , S.totalDrCr            = 1
            , S.totalCmdty           = 1 }

