-- | Options when using the Posts report with the Zinc command line
-- interface.
module Penny.Cabin.Posts.ZincOpts where

import qualified Penny.Copper as Cop
import qualified Penny.Cabin.Colors as C
import qualified Penny.Cabin.Options as O
import qualified Penny.Cabin.Posts.Allocate as A
import qualified Penny.Cabin.Posts.Fields as F
import qualified Penny.Cabin.Posts.Spacers as S
import Penny.Cabin.Posts.Meta (Box)
import qualified Penny.Lincoln as L
import qualified Data.Text as X

-- | Provides a type that is used to configure the compile-time
-- defaults for the Posts report. Some of the aspects of the Postings
-- report that can be configured in the Options module make no sense
-- to be included in this type, as they will always be overriden by
-- the command line (for example, the default matcher will always
-- depend on what has already been seen on the command line.) Thus
-- this type includes only the aspects of the Postings report that can
-- be configured at compile type when being used with Zinc.
data ZincOpts =
  ZincOpts { drCrColors :: C.DrCrColors
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

