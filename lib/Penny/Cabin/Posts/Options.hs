-- | Options for the Posts report.
module Penny.Cabin.Posts.Options where


import Control.Monad.Exception.Synchronous (Exceptional)
import Data.Text (Text)
import qualified Data.Text as X
import Data.Time (formatTime)
import System.Locale (defaultTimeLocale)
import qualified Data.Time as Time
import qualified Text.Matchers.Text as M

import qualified Penny.Liberty as Ly
import qualified Penny.Liberty.Expressions as Ex
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Balance as Bal
import qualified Penny.Lincoln.Bits as Bits
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Cabin.Posts.Allocate as A
import qualified Penny.Cabin.Chunk as CC
import qualified Penny.Cabin.Colors as C
import qualified Penny.Cabin.Options as O
import qualified Penny.Cabin.Posts.Fields as Fields
import qualified Penny.Cabin.Posts.Fields as F
import qualified Penny.Cabin.Posts.Meta as Meta
import qualified Penny.Cabin.Posts.Spacers as S
import qualified Penny.Cabin.Posts.Spacers as Spacers
import qualified Penny.Cabin.Colors.DarkBackground as Dark
import Penny.Copper.DateTime (DefaultTimeZone)
import Penny.Copper.Qty (RadGroup)
import qualified Penny.Shield as S


type Box = L.Box Meta.PostMeta

data T =
  T { drCrColors :: C.DrCrColors
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
      -- function is applied to a PostingInfo so it has lots of
      -- information, but it should return a formatted string of
      -- the quantity. Allows you to format digit grouping,
      -- radix points, perform rounding, etc.
      
    , balanceFormat :: Bits.Commodity -> Bal.BottomLine -> X.Text
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

    , colorPref :: CC.Colors
      -- ^ How many colors you want to see, or do it
      -- automatically.

    , timeZone :: DefaultTimeZone
      -- ^ When dates and times are given on the command line
      -- and they have no time zone, they are assumed to be in
      -- this time zone. This has no bearing on how dates are
      -- formatted in the output; for that, see dateFormat
      -- above.

    , radGroup :: RadGroup
      -- ^ The characters used for the radix point for numbers
      -- given on the command line (e.g. a full stop, or a
      -- comma) and for the digit group separator for
      -- numbers parsed from the command line (e.g. a full stop,
      -- or a comma). Affects how inputs are parsed. Has no bearing
      -- on how output is formatted; for that, see qtyFormat and
      -- balanceFormat above.
      
    , sensitive :: M.CaseSensitive
      -- ^ Whether pattern matches are case sensitive by default.
      
    , factory :: M.CaseSensitive
                 -> Text -> Exceptional Text (Text -> Bool)
      -- ^ Default factory for pattern matching
      
    , tokens :: [Ex.Token (L.Box Ly.LibertyMeta -> Bool)]
      -- ^ Default list of tokens used to filter postings.
      
    , postFilter :: [Ly.PostFilterFn]
      -- ^ The entire posting list is transformed by this
      -- function after it is filtered according to the tokens.
      
    , fields :: Fields.T Bool
      -- ^ Default fields to show in the report.
      
    , spacers :: Spacers.T Int
      -- ^ Default width for spacer fields. If any of these Ints are
      -- less than or equal to zero, there will be no spacer. There is
      -- never a spacer for fields that do not appear in the report.
      
    , showZeroBalances :: O.ShowZeroBalances
    }

newtype ReportWidth = ReportWidth { unReportWidth :: Int }
                      deriving (Eq, Show, Ord)

ymd :: Box -> X.Text
ymd p = X.pack (formatTime defaultTimeLocale fmt d) where
  d = Time.localDay
      . Bits.localTime
      . Q.dateTime
      . L.boxPostFam
      $ p
  fmt = "%Y-%m-%d"

qtyAsIs :: Box -> X.Text
qtyAsIs p = X.pack . show . Bits.unQty . Q.qty . L.boxPostFam $ p

balanceAsIs :: Bits.Commodity -> Bal.BottomLine -> X.Text
balanceAsIs _ n = case n of
  Bal.Zero -> X.pack "--"
  Bal.NonZero c -> X.pack . show . Bits.unQty . Bal.qty $ c

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
  -> RadGroup
  -> S.Runtime
  -> T
defaultOptions dtz rg rt =
  T { drCrColors = Dark.drCrColors
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
    , sensitive = M.Insensitive
    , factory = \s t -> (return (M.within s t))
    , tokens = []
    , postFilter = []
    , fields = defaultFields
    , spacers = defaultSpacerWidth
    , showZeroBalances = O.ShowZeroBalances False }

widthFromRuntime :: S.Runtime -> ReportWidth
widthFromRuntime rt = case S.screenWidth rt of
  Nothing -> defaultWidth
  Just w -> ReportWidth . S.unScreenWidth $ w

defaultFields :: Fields.T Bool
defaultFields =
  Fields.T { F.globalTransaction    = False
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

defaultSpacerWidth :: Spacers.T Int
defaultSpacerWidth =
  Spacers.T { S.globalTransaction    = 1
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

