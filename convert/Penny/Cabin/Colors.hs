-- | Default colors for use in all Cabin reports. These tie the
-- specific colors given in the Chunk module to the semantic elements
-- of the Cabin reports.
module Penny.Cabin.Colors where

import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Balance as Bal
import qualified Penny.Lincoln.Bits as Bits
import qualified Penny.Cabin.Chunk as C
import qualified Penny.Cabin.Meta as M

-- | The colors for debits and credits. These are used for the entry's
-- amount and for the total amounts that accompany each entry. Here,
-- @even@ and @odd@ refer to the the posting's visible number. @Zero@
-- is for the balance entries whose balance is zero.
data DrCrColors =
  DrCrColors { evenDebit :: !C.TextSpec
             , evenCredit :: !C.TextSpec
             , oddDebit :: !C.TextSpec
             , oddCredit :: !C.TextSpec
             , evenZero :: !C.TextSpec
             , oddZero :: !C.TextSpec }

-- | The colors for all fields other than entries and totals.
data BaseColors =
  BaseColors { evenColors :: !C.TextSpec
             , oddColors :: !C.TextSpec }

-- | Change a BaseColors to a TextSpec.
colors :: M.VisibleNum -> BaseColors -> C.TextSpec
colors vn b = let n = L.forward . M.unVisibleNum $ vn in
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
  
-- | TextSpec to use when showing the lack of a balance.
noBalanceColors :: M.VisibleNum -> DrCrColors -> C.TextSpec
noBalanceColors vn dc =
  if odd . L.forward . M.unVisibleNum $ vn
  then oddZero dc
  else evenZero dc
