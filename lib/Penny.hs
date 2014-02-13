-- | Penny - extensible double-entry accounting system

module Penny
  ( -- * Building a custom Penny binary

    -- | Everything you need to create a custom Penny program is
    -- available by importing only this module.
    Version(..)
  , Defaults(..)
  , Z.Matcher(..)

  -- ** Color schemes
  , E.Scheme(..)
  , E.Changers
  , E.Labels(..)
  , E.EvenAndOdd(..)
  , module System.Console.Rainbow

  -- ** Sorting
  , Z.SortField(..)
  , CabP.SortOrder(..)

  -- ** Expression type
  , Exp.ExprDesc(..)

  -- ** Convert report options
  , Target(..)
  , CP.SortBy(..)

  -- ** Postings report options
  , Fields(..)
  , Spacers(..)
  , widthFromRuntime
  , Ps.yearMonthDay

  -- ** Formatting quantities
  , S3(..)
  , FormatQty
  , qtyFormatter
  , getQtyFormat
  , L.Radix(..)
  , L.PeriodGrp(..)
  , L.CommaGrp(..)

  -- ** Runtime
  , S.Runtime
  , S.environment

  -- ** Text
  , X.Text
  , X.pack

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
    -- "Penny.Shield" - the Penny runtime environment. Depends on
    -- Lincoln.
    --
    -- "Penny.Steel" - independent utilities. Depends on no other
    -- Penny components.
    --
    -- "Penny.Wheat" - helping you build your own programs to check
    -- your ledger.  Depends on Steel, Lincoln, and Copper.
    --
    -- "Penny.Zinc" - the Penny command-line interface. Depends on
    -- Cabin, Copper, Liberty, and Lincoln.
    --
    -- The dependencies are represented as a dot file in
    -- @doc\/dependencies.dot@ in the Penny git repository.
  ) where

import Data.Ord (comparing)
import Data.List (sortBy, groupBy)
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Text as X
import qualified Data.Map as Map
import Data.Version (Version(..))
import qualified Penny.Cabin.Balance.Convert as Conv
import qualified Penny.Cabin.Balance.Convert.Parser as CP
import qualified Penny.Cabin.Balance.Convert.Options as ConvOpts
import qualified Penny.Cabin.Balance.MultiCommodity as MC
import qualified Penny.Cabin.Balance.MultiCommodity.Parser as MP
import System.Console.Rainbow
import qualified Penny.Cabin.Interface as I
import qualified Penny.Cabin.Options as CO
import qualified Penny.Cabin.Parsers as CabP
import qualified Penny.Cabin.Posts as Ps
import qualified Penny.Cabin.Posts.Fields as PF
import qualified Penny.Cabin.Posts.Spacers as PS
import qualified Penny.Cabin.Posts.Meta as M
import qualified Penny.Cabin.Scheme as E
import qualified Penny.Copper as Cop
import Data.Sums
import qualified Data.Sums as Su
import qualified Penny.Lincoln as L
import qualified Data.Prednote.Expressions as Exp
import qualified Penny.Zinc as Z
import qualified Penny.Shield as S
import qualified Text.Matchers as Mr

-- | A function used to format quantities.
type FormatQty
  = [Cop.LedgerItem]
  -- ^ All parsed items

  -> L.Amount L.Qty
  -> X.Text

-- | This type contains settings for all the reports, as well as
-- default settings for the global options. Some of these can be
-- overridden on the command line.
data Defaults = Defaults
  { caseSensitive :: Bool
    -- ^ Whether the matcher is case sensitive by default

  , matcher :: Z.Matcher
    -- ^ Which matcher to use

  , colorToFile :: Bool
    -- ^ Use colors when standard output is not a terminal?

  , expressionType :: Exp.ExprDesc
    -- ^ Use RPN or infix expressions? This affects both the posting
    -- filter and the filter for the Postings report.

  , defaultScheme :: Maybe E.Scheme
    -- ^ Default color scheme. If Nothing, there is no default color
    -- scheme. If there is no default color scheme and the user does
    -- not pick one on the command line, no colors will be used.

  , additionalSchemes :: [E.Scheme]
    -- ^ Additional color schemes the user can pick from on the
    -- command line.

  , sorter :: [(Z.SortField, CabP.SortOrder)]
    -- ^ Postings are sorted in this order by default. For example, if
    -- the first pair is (Date, Ascending), then postings are first
    -- sorted by date in ascending order. If the second pair is
    -- (Payee, Ascending), then postings with the same date are then
    -- sorted by payee.
    --
    -- If this list is empty, then by default postings are left in the
    -- same order as they appear in the ledger files.

  , formatQty :: FormatQty
    -- ^ How to format quantities. This affects only quantities that
    -- are not parsed from the ledger.  Examples include calculated
    -- totals and inferred quantities.  Affects all reports.

  , balanceShowZeroBalances :: Bool
    -- ^ Show zero balances in the balance report? If True, show them;
    -- if False, hide them.

  , balanceOrder :: CabP.SortOrder
    -- ^ Whether to sort the accounts in ascending or descending order
    -- by account name in the balance report.

  , convertShowZeroBalances :: Bool
    -- ^ Show zero balances in the convert report? If True, show them;
    -- if False, hide them.

  , convertTarget :: Target
    -- ^ The commodity to which to convert the commodities in the
    -- convert report.

  , convertOrder :: CabP.SortOrder
    -- ^ Sort the convert report in ascending or descending order.

  , convertSortBy :: CP.SortBy
    -- ^ Sort by account or by quantity in the convert report.

  , postingsFields :: Fields Bool
    -- ^ Fields to show by default in the postings report.

  , postingsWidth :: Int
    -- ^ The postings report is roughly this wide by
    -- default. Typically this will be as wide as your terminal.

  , postingsShowZeroBalances :: Bool
    -- ^ Show zero balances in the postings report? If True, show
    -- them; if False, hide them.

  , postingsDateFormat :: (M.PostMeta, L.Posting) -> X.Text
    -- ^ How to format dates in the postings report.

  , postingsSubAccountLength :: Int
    -- ^ Account names in the postings report are shortened if
    -- necessary in order to help the report fit within the allotted
    -- width (see postingsWidth). Account names are only shortened as
    -- much as is necessary for them to fit; however, each sub-account
    -- name will not be shortened any more than the amount given here.

  , postingsPayeeAllocation :: Int
    -- ^ postingsPayeeAllocation and postingsAccountAllocation
    -- determine how much space is allotted to the payee and account
    -- fields in the postings report. These fields are variable
    -- width. After space for most other fields is allotted, space is
    -- allotted for these two fields. The two fields divide the space
    -- proportionally depending on postingsPayeeAllocation and
    -- postingsAccountAllocation. For example, if
    -- postingsPayeeAllocation is 60 and postingsAccountAllocation is
    -- 40, then the payee field gets 60 percent of the leftover space
    -- and the account field gets 40 percent of the leftover space.
    --
    -- Both postingsPayeeAllocation and postingsAccountAllocation
    -- must be positive integers; if either one is less than 1, your
    -- program will crash at runtime.

  , postingsAccountAllocation :: Int
    -- ^ See postingsPayeeAllocation above for an explanation

  , postingsSpacers :: Spacers Int
    -- ^ Determines the number of spaces that appears to the right of
    -- each named field; for example, sPayee indicates how many spaces
    -- will appear to the right of the payee field. Each field of the
    -- Spacers should be a non-negative integer (although currently
    -- the absolute value of the field is taken.)
  }

-- | Provides a function to use in the 'formatQty' field. This formats
-- quantities that were not parsed in the ledger.  It first consults a
-- list of all items that were parsed from the ledger.  It examines
-- these items to determine if another item with the same commodity
-- already exists in the ledger.
--
-- If other items with the same commodity exist in the ledger, the
-- radix point most frequently occurring amongst those items is
-- used. If at least one of these items (with this radix point) also
-- has grouped digits, then the quantity will be formatted with
-- grouped digits; otherwise, no digit grouping is performed. If digit
-- grouping is performed, it is done according to the following rules:
--
-- * only digits to the left of the radix point are grouped
--
-- * grouping is performed only if the number has at least five
-- digits. Therefore, 1234 is not grouped, but 1,234.5 is grouped, as
-- is 12,345
--
-- * the character most frequently appearing as a grouping character
-- (for this particular commodity and radix point) is used to perform
-- grouping
--
-- * digits are grouped into groups of 3 digits
--
-- If a radix point cannot be determined from the quantities for a
-- given commodity, then the radix point appearing most frequently for
-- all commodities is used.  If it's impossible to determine a radix
-- point from all commodities, then the given default radix point and
-- digit grouping (if desired) is used.
--
-- This function builds a map internally which holds all the
-- formatting information; it might be expensive to build, so the
-- function is written to be partially applied.

qtyFormatter
  :: Su.S3 L.Radix L.PeriodGrp L.CommaGrp
  -- ^ What to do if no radix or grouping information can be
  -- determined from the ledger.  Pass Radix if you want to use a
  -- radix point but no grouping; a PeriodGrp if you want to use a
  -- period for a radix point and the given grouping character, or a
  -- CommaGrp if you want to use a comma for a radix point and the
  -- given grouping character.

  -> FormatQty
qtyFormatter df ls =
  let getFmt = getQtyFormat df ls
  in \a -> L.showQtyRep . L.qtyToRep (getFmt a) . L.qty $ a

-- | Obtains radix and grouping information for a particular commodity
-- and quantity, but does not actually perform the formatting.
getQtyFormat
  :: Su.S3 L.Radix L.PeriodGrp L.CommaGrp
  -- ^ What to do if no radix or grouping information can be
  -- determined from the ledger.  Pass Radix if you want to use a
  -- radix point but no grouping; a PeriodGrp if you want to use a
  -- period for a radix point and the given grouping character, or a
  -- CommaGrp if you want to use a comma for a radix point and the
  -- given grouping character.

  -> [Cop.LedgerItem]
  -> L.Amount L.Qty
  -> Su.S3 L.Radix L.PeriodGrp L.CommaGrp
getQtyFormat df ls =
  let m = formattingMap ls
  in \a -> fromMaybe df (Map.lookup (L.commodity a) m)


-- | Returns a map of each commodity in the ledger and the grouping to
-- use for it.
formattingMap
  :: [Cop.LedgerItem]
  -> Map.Map L.Commodity (Su.S3 L.Radix L.PeriodGrp L.CommaGrp)
formattingMap
  = Map.fromList
  . mapMaybe formatCmdty
  . groupBy (\x y -> fst x == fst y)
  . sortBy (comparing fst)
  . allQtyRep

-- | Given a list of (Commodity, QtyRep) pairs, get a single pair of
-- the commodity and the grouping that should be used for this
-- commodity. The input list must not be empty. Returns Nothing if no
-- group data can be ascertained.
formatCmdty
  :: [(L.Commodity, L.QtyRep)]
  -> Maybe (L.Commodity, Su.S3 L.Radix L.PeriodGrp L.CommaGrp)
formatCmdty ls = case L.bestRadGroup . map snd $ ls of
  Nothing -> Nothing
  Just r -> Just (fst . head $ ls, r)

-- | Given a list of LedgerItem, create a list of pairs of commodities
-- and QtyRep.
allQtyRep :: [Cop.LedgerItem] -> [(L.Commodity, L.QtyRep)]
allQtyRep = concatMap toPairs
  where
    toPairs i = case i of
      Su.S4a t ->
        mapMaybe toEntPair
        . L.unEnts
        . snd
        . L.unTransaction
        $ t
      Su.S4b p ->
        [( L.unTo . L.to . L.price $ p
        , L.unCountPerUnit . L.countPerUnit . L.price $ p)]
      _ -> []

toEntPair :: L.Ent m -> Maybe (L.Commodity, L.QtyRep)
toEntPair e = case L.entry e of
  Left en -> Just (L.commodity . L.amount $ en, L.qty . L.amount $ en)
  Right _ -> Nothing

-- | Creates an IO action that you can use for the main function.
runPenny
  :: Version
  -- ^ Version of the executable
  -> (S.Runtime -> Defaults)
     -- ^ runPenny will apply this function to the Runtime. This way
     -- the defaults you use can vary depending on environment
     -- variables, the terminal type, the date, etc.
  -> IO ()
runPenny ver getDefaults = do
  rt <- S.runtime
  let df = getDefaults rt
      rs = allReports df
  Z.runZinc ver (toZincDefaults df) rt rs

-- | The commodity to which to convert the commodities in the convert
-- report.
data Target
  = AutoTarget
    -- ^ Selects a target commodity automatically, based on which
    -- commodity is the most common target commodity in the prices in
    -- your ledger files. If there is a tie for most common target
    -- commodity, the target that appears later in your ledger files
    -- is used.
  | ManualTarget String
    -- ^ Always uses the commodity named by the string given.
  deriving Show

-- | Gets the current screen width from the runtime. If the COLUMNS
-- environment variable is not set, uses 80.
widthFromRuntime :: S.Runtime -> Int
widthFromRuntime rt = case S.screenWidth rt of
  Nothing -> 80
  Just sw -> S.unScreenWidth sw

convTarget :: Target -> CP.Target
convTarget t = case t of
  AutoTarget -> CP.AutoTarget
  ManualTarget s -> CP.ManualTarget . L.To . L.Commodity . X.pack $ s

allReports
  :: Defaults
  -> [I.Report]
allReports df =
  let bd = toBalanceDefaults df
      cd = toConvertDefaults df
      pd = toPostingsDefaults df
  in [ Ps.zincReport pd
     , MC.parseReport bd
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
  , Z.exprDesc = expressionType d
  , Z.formatQty = formatQty d
  }

toBalanceDefaults :: Defaults -> MP.ParseOpts
toBalanceDefaults d = MP.ParseOpts
  { MP.showZeroBalances =
      CO.ShowZeroBalances . balanceShowZeroBalances $ d
  , MP.order = balanceOrder d
  }

toConvertDefaults :: Defaults -> ConvOpts.DefaultOpts
toConvertDefaults d = ConvOpts.DefaultOpts
  { ConvOpts.showZeroBalances =
      CO.ShowZeroBalances . convertShowZeroBalances $ d
  , ConvOpts.target = convTarget . convertTarget $ d
  , ConvOpts.sortOrder = convertOrder d
  , ConvOpts.sortBy = convertSortBy d
  }

toPostingsDefaults :: Defaults -> Ps.ZincOpts
toPostingsDefaults d = Ps.ZincOpts
  { Ps.fields = convFields . postingsFields $ d
  , Ps.width = Ps.ReportWidth . postingsWidth $ d
  , Ps.showZeroBalances =
      CO.ShowZeroBalances . postingsShowZeroBalances $ d
  , Ps.dateFormat = postingsDateFormat d
  , Ps.subAccountLength =
      Ps.SubAccountLength . postingsSubAccountLength $ d
  , Ps.payeeAllocation =
      Ps.alloc . postingsPayeeAllocation $ d
  , Ps.accountAllocation =
      Ps.alloc . postingsAccountAllocation $ d
  , Ps.spacers = convSpacers . postingsSpacers $ d
  }

data Spacers a = Spacers
  { sGlobalTransaction :: a
  , sRevGlobalTransaction :: a
  , sGlobalPosting :: a
  , sRevGlobalPosting :: a
  , sFileTransaction :: a
  , sRevFileTransaction :: a
  , sFilePosting :: a
  , sRevFilePosting :: a
  , sFiltered :: a
  , sRevFiltered :: a
  , sSorted :: a
  , sRevSorted :: a
  , sVisible :: a
  , sRevVisible :: a
  , sLineNum :: a
  , sDate :: a
  , sFlag :: a
  , sNumber :: a
  , sPayee :: a
  , sAccount :: a
  , sPostingDrCr :: a
  , sPostingCmdty :: a
  , sPostingQty :: a
  , sTotalDrCr :: a
  , sTotalCmdty :: a
  } deriving (Show, Eq)

data Fields a = Fields
  { fGlobalTransaction :: a
  , fRevGlobalTransaction :: a
  , fGlobalPosting :: a
  , fRevGlobalPosting :: a
  , fFileTransaction :: a
  , fRevFileTransaction :: a
  , fFilePosting :: a
  , fRevFilePosting :: a
  , fFiltered :: a
  , fRevFiltered :: a
  , fSorted :: a
  , fRevSorted :: a
  , fVisible :: a
  , fRevVisible :: a
  , fLineNum :: a
  , fDate :: a
  , fFlag :: a
  , fNumber :: a
  , fPayee :: a
  , fAccount :: a
  , fPostingDrCr :: a
  , fPostingCmdty :: a
  , fPostingQty :: a
  , fTotalDrCr :: a
  , fTotalCmdty :: a
  , fTotalQty :: a
  , fTags :: a
  , fMemo :: a
  , fFilename :: a
  } deriving (Show, Eq)

convSpacers :: Spacers a -> PS.Spacers a
convSpacers s = PS.Spacers
  { PS.globalTransaction = sGlobalTransaction s
  , PS.revGlobalTransaction = sRevGlobalTransaction s
  , PS.globalPosting = sGlobalPosting s
  , PS.revGlobalPosting = sRevGlobalPosting s
  , PS.fileTransaction = sFileTransaction s
  , PS.revFileTransaction = sRevFileTransaction s
  , PS.filePosting = sFilePosting s
  , PS.revFilePosting = sRevFilePosting s
  , PS.filtered = sFiltered s
  , PS.revFiltered = sRevFiltered s
  , PS.sorted = sSorted s
  , PS.revSorted = sRevSorted s
  , PS.visible = sVisible s
  , PS.revVisible = sRevVisible s
  , PS.lineNum = sLineNum s
  , PS.date = sDate s
  , PS.flag = sFlag s
  , PS.number = sNumber s
  , PS.payee = sPayee s
  , PS.account = sAccount s
  , PS.postingDrCr = sPostingDrCr s
  , PS.postingCmdty = sPostingCmdty s
  , PS.postingQty = sPostingQty s
  , PS.totalDrCr = sTotalDrCr s
  , PS.totalCmdty = sTotalCmdty s
  }

convFields :: Fields a -> PF.Fields a
convFields f = PF.Fields
  { PF.globalTransaction = fGlobalTransaction f
  , PF.revGlobalTransaction = fRevGlobalTransaction f
  , PF.globalPosting = fGlobalPosting f
  , PF.revGlobalPosting = fRevGlobalPosting f
  , PF.fileTransaction = fFileTransaction f
  , PF.revFileTransaction = fRevFileTransaction f
  , PF.filePosting = fFilePosting f
  , PF.revFilePosting = fRevFilePosting f
  , PF.filtered = fFiltered f
  , PF.revFiltered = fRevFiltered f
  , PF.sorted = fSorted f
  , PF.revSorted = fRevSorted f
  , PF.visible = fVisible f
  , PF.revVisible = fRevVisible f
  , PF.lineNum = fLineNum f
  , PF.date = fDate f
  , PF.flag = fFlag f
  , PF.number = fNumber f
  , PF.payee = fPayee f
  , PF.account = fAccount f
  , PF.postingDrCr = fPostingDrCr f
  , PF.postingCmdty = fPostingCmdty f
  , PF.postingQty = fPostingQty f
  , PF.totalDrCr = fTotalDrCr f
  , PF.totalCmdty = fTotalCmdty f
  , PF.totalQty = fTotalQty f
  , PF.tags = fTags f
  , PF.memo = fMemo f
  , PF.filename = fFilename f
  }
