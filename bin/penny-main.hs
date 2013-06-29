module Main where

import Penny
import qualified Paths_penny_bin as PPB

-- | This type contains settings for all the reports, as well as
-- default settings for the global options. Some of these can be
-- overridden on the command line.
defaults :: Runtime -> Defaults
defaults rt = Defaults
  { caseSensitive = False
    -- ^ Whether the matcher is case sensitive by default

  , matcher = Within
    -- ^ Which matcher to use. Your choices:
    -- Within
    -- Exact
    -- TDFA (regular expressions, POSIX semantics)
    -- PCRE (regular expressions, PCRE semantics)

  , colorToFile = False
    -- ^ Use colors when standard output is not a terminal?

  , expressionType = Infix
    -- ^ Use Infix or RPN expressions for the posting filters and for
    -- the filters in the Postings report? Change to @RPN@ if you like
    -- that kind of thing (I find RPN easier to enter; if you're not
    -- familiar with it, you might become a convert.)

  , defaultScheme = Just schemeDark
    -- ^ Default color scheme. If Nothing, there is no default color
    -- scheme. If there is no default color scheme and the user does
    -- not pick one on the command line, no colors will be used.

  , additionalSchemes = [schemeDark, schemeLight, schemePlain]
    -- ^ Additional color schemes the user can pick from on the
    -- command line.

  , sorter = [(Date, Ascending)]
    -- ^ Postings are sorted in this order by default. For example, if
    -- the first pair is (Date, Ascending), then postings are first
    -- sorted by date in ascending order. If the second pair is
    -- (Payee, Ascending), then postings with the same date are then
    -- sorted by payee.
    --
    -- If this list is empty, then by default postings are left in the
    -- same order as they appear in the ledger files.

  , balanceFormat = const defaultQtyFormat
    -- ^ How to format balances in the balance report. Change this
    -- function if, for example, you want to allow for digit
    -- grouping. The default function does not perform digit grouping.

  , balanceShowZeroBalances = False
    -- ^ Show zero balances in the balance report? If True, show them;
    -- if False, hide them.

  , balanceOrder = Ascending
    -- ^ Whether to sort the accounts in ascending or descending order
    -- by account name in the balance report. Your choices: Ascending
    -- or Descending.

  , convertShowZeroBalances = False
    -- ^ Show zero balances in the convert report? If True, show them;
    -- if False, hide them.

  , convertTarget = AutoTarget
    -- ^ The commodity to which to convert the commodities in the
    -- convert report. Your choices:
    --
    -- AutoTarget - selects a target commodity automatically, based on
    -- which commodity is the most common target commodity in the
    -- prices in your ledger files. If there is a tie for most common
    -- target commodity, the target that appears later in your ledger
    -- files is used.
    --
    -- ManualTarget CMDTY_NAME - always use the given commodity.

  , convertOrder = Ascending
    -- ^ Sort the convert report in ascending or descending order.

  , convertSortBy = SortByName
    -- ^ Sort by account or by quantity in the convert report. Your
    -- choices:
    --
    -- SortByQty
    -- SortByName

  , convertFormat = const defaultQtyFormat
    -- ^ How to format balances in the convert report. For instance,
    -- this function might perform digit grouping. The default
    -- function does not perform any digit grouping.

  , postingsFields = fields
    -- ^ Fields to show by default in the postings report.

  , postingsWidth = widthFromRuntime rt
    -- ^ The postings report is roughly this wide by default. Use
    -- @widthFromRuntime rt@ if you want to use the current width of
    -- your terminal.

  , postingsShowZeroBalances = False
    -- ^ Show zero balances in the postings report? If True, show
    -- them; if False, hide them.

  , postingsDateFormat = yearMonthDay
    -- ^ How to format dates in the postings report.

  , postingsQtyFormat = qtyAsIs
    -- ^ How to format quantities in the balance report. This function
    -- is used when showing the quantity for the posting itself, and
    -- not the quantity for the totals columns (for that, see
    -- postingsBalanceFormat.) For example this function might perform
    -- digit grouping.

  , postingsBalanceFormat = balanceAsIs
    -- ^ How to format balance totals in the postings report.

  , postingsSubAccountLength = 2
    -- ^ Account names in the postings report are shortened if
    -- necessary in order to help the report fit within the allotted
    -- width (see postingsWidth). Account names are only shortened as
    -- much as is necessary for them to fit; however, each sub-account
    -- name will not be shortened any more than the amount given here.
    --
    -- This number should be a non-negative integer.

  , postingsPayeeAllocation = 40
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

  , postingsAccountAllocation = 60
    -- ^ See postingsPayeeAllocation above for an explanation

  , postingsSpacers = spacers
    -- ^ Determines the number of spaces that appears to the right of
    -- each named field; for example, sPayee indicates how many spaces
    -- will appear to the right of the payee field. Each field of the
    -- Spacers should be a non-negative integer (although currently
    -- the absolute value of the field is taken.)
  }

-- | Controls which fields appear in the report by default.
fields :: Fields Bool
fields = Fields
  { fGlobalTransaction = False
  , fRevGlobalTransaction = False
  , fGlobalPosting = False
  , fRevGlobalPosting = False
  , fFileTransaction = False
  , fRevFileTransaction = False
  , fFilePosting = False
  , fRevFilePosting = False
  , fFiltered = False
  , fRevFiltered = False
  , fSorted = False
  , fRevSorted = False
  , fVisible = False
  , fRevVisible = False
  , fLineNum = False
  , fDate = True
  , fFlag = False
  , fNumber = False
  , fPayee = True
  , fAccount = True
  , fPostingDrCr = True
  , fPostingCmdty = True
  , fPostingQty = True
  , fTotalDrCr = True
  , fTotalCmdty = True
  , fTotalQty = True
  , fTags = False
  , fMemo = False
  , fFilename = False
  }

-- | Controls how many spaces appear to the right of each named field.
spacers :: Spacers Int
spacers = Spacers
  { sGlobalTransaction = 1
  , sRevGlobalTransaction = 1
  , sGlobalPosting = 1
  , sRevGlobalPosting = 1
  , sFileTransaction = 1
  , sRevFileTransaction = 1
  , sFilePosting = 1
  , sRevFilePosting = 1
  , sFiltered = 1
  , sRevFiltered = 1
  , sSorted = 1
  , sRevSorted = 1
  , sVisible = 1
  , sRevVisible = 1
  , sLineNum = 1
  , sDate = 1
  , sFlag = 1
  , sNumber = 1
  , sPayee = 4
  , sAccount = 1
  , sPostingDrCr = 1
  , sPostingCmdty = 1
  , sPostingQty = 1
  , sTotalDrCr = 1
  , sTotalCmdty = 1
  }

-- | The light color scheme. You can change various values below to
-- affect the color scheme.
schemeLight :: Scheme
schemeLight = Scheme "light" "for light background terminals"
              lightLabels

lightLabels :: Labels (EvenAndOdd (Chunk -> Chunk))
lightLabels = Labels
  { debit = EvenAndOdd { eoEven = lightDebit lightEvenTextSpec
                       , eoOdd = lightDebit lightOddTextSpec }
  , credit = EvenAndOdd { eoEven = lightCredit lightEvenTextSpec
                        , eoOdd = lightCredit lightOddTextSpec }
  , zero = EvenAndOdd { eoEven = lightZero lightEvenTextSpec
                      , eoOdd = lightZero lightOddTextSpec }
  , other = EvenAndOdd { eoEven = lightEvenTextSpec
                       , eoOdd = lightOddTextSpec }
  }

lightEvenTextSpec :: Chunk -> Chunk
lightEvenTextSpec = id

lightOddTextSpec :: Chunk -> Chunk
lightOddTextSpec = (<> (c8_b_default <> c256_b_255))

lightDebit :: (Chunk -> Chunk) -> Chunk -> Chunk
lightDebit f c = f c <> c8_f_magenta <> c256_f_52

lightCredit :: (Chunk -> Chunk) -> Chunk -> Chunk
lightCredit f c = f c <> c8_f_cyan <> c256_f_21

lightZero :: (Chunk -> Chunk) -> Chunk -> Chunk
lightZero f c = f c <> c8_f_black <> c256_f_0

-- | The dark color scheme. You can change various values below to
-- affect the color scheme.
schemeDark :: Scheme
schemeDark = Scheme "dark" "for dark background terminals"
              darkLabels

darkLabels :: Labels (EvenAndOdd (Chunk -> Chunk))
darkLabels = Labels
  { debit = EvenAndOdd { eoEven = darkDebit darkEvenTextSpec
                       , eoOdd = darkDebit darkOddTextSpec }
  , credit = EvenAndOdd { eoEven = darkCredit darkEvenTextSpec
                        , eoOdd = darkCredit darkOddTextSpec }
  , zero = EvenAndOdd { eoEven = darkZero darkEvenTextSpec
                      , eoOdd = darkZero darkOddTextSpec }
  , other = EvenAndOdd { eoEven = darkEvenTextSpec
                       , eoOdd = darkOddTextSpec }
  }

darkEvenTextSpec :: Chunk -> Chunk
darkEvenTextSpec = id

darkOddTextSpec :: Chunk -> Chunk
darkOddTextSpec = (<> (c8_b_default <> c256_b_235))

darkDebit :: (Chunk -> Chunk) -> Chunk -> Chunk
darkDebit f c = f c <> c8_f_magenta <> c256_f_208

darkCredit :: (Chunk -> Chunk) -> Chunk -> Chunk
darkCredit f c = f c <> c8_f_cyan <> c256_f_45

darkZero :: (Chunk -> Chunk) -> Chunk -> Chunk
darkZero f c = f c <> c8_f_white <> c256_f_15


-- | Plain scheme has no colors at all.
schemePlain :: Scheme
schemePlain = Scheme "plain" "uses default terminal colors"
              plainLabels

plainLabels :: Labels (EvenAndOdd (Chunk -> Chunk))
plainLabels = Labels
  { debit = EvenAndOdd id id
  , credit = EvenAndOdd id id
  , zero = EvenAndOdd id id
  , other = EvenAndOdd id id
  }

main :: IO ()
main = runPenny PPB.version defaults

