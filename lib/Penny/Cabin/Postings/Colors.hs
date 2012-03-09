-- | Colors for the Postings report. These augment the features in
-- "Penny.Cabin.Colors" to specify colors specifically for the
-- Postings report.
module Penny.Cabin.Postings.Colors where

import qualified Penny.Lincoln.Balance as Bal
import qualified Penny.Lincoln.Bits as Bits
import qualified Penny.Cabin.Colors as C
import qualified Penny.Cabin.Postings.Types as T

-- | The colors for debits and credits. These are used for the entry's
-- amount and for the total amounts that accompany each entry. Here,
-- @even@ and @odd@ refer to the the posting's visible number. @Zero@
-- is for the balance entries whose balance is zero.
data DrCrColors =
  DrCrColors { evenDebit :: C.TextSpec
             , evenCredit :: C.TextSpec
             , oddDebit :: C.TextSpec
             , oddCredit :: C.TextSpec
             , evenZero :: C.TextSpec
             , oddZero :: C.TextSpec }

-- | The colors for all fields other than entries and totals.
data BaseColors =
  BaseColors { evenColors :: C.TextSpec
             , oddColors :: C.TextSpec }

-- | Change a BaseColors to a TextSpec.
colors :: T.VisibleNum -> BaseColors -> C.TextSpec
colors vn b = let n = T.unVisibleNum $ vn in
  if odd n then oddColors b else evenColors b

-- | Change a DrCrColors to a BaseColors; you can then use 'colors' to
-- change that to a TextSpec.
drCrToBaseColors :: Bits.DrCr -> DrCrColors -> BaseColors
drCrToBaseColors dc col = case dc of
  Bits.Debit -> BaseColors (evenDebit col) (oddDebit col)
  Bits.Credit -> BaseColors (evenCredit col) (oddCredit col)

-- | Change a DrCrColors to a BaseColors, based on a balance (unlike
-- entries, balances might be zero.)
bottomLineToBaseColors :: DrCrColors -> Bal.BottomLine -> BaseColors
bottomLineToBaseColors col no = case no of
  Bal.Zero -> BaseColors (evenZero col) (oddZero col)
  Bal.NonZero column -> drCrToBaseColors (Bal.drCr column) col
  
