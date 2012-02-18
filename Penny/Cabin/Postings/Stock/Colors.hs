module Penny.Cabin.Postings.Stock.Colors ( 
  C.Width(Width, unWidth),
  C.TextSpec,
  C.chunk,
  colors,
  drCrToBaseColors,
  noughtToBaseColors,
  DrCrColors(DrCrColors, evenDebit, evenCredit, oddDebit, oddCredit),
  BaseColors(BaseColors, evenColors, oddColors)) where

import qualified Penny.Lincoln.Balance as Bal
import qualified Penny.Lincoln.Bits as Bits
import qualified Penny.Cabin.Postings.Base.Base as B
import qualified Penny.Cabin.Colors as C

data DrCrColors =
  DrCrColors { evenDebit :: C.TextSpec
             , evenCredit :: C.TextSpec
             , oddDebit :: C.TextSpec
             , oddCredit :: C.TextSpec
             , evenZero :: C.TextSpec
             , oddZero :: C.TextSpec }

data BaseColors =
  BaseColors { evenColors :: C.TextSpec
             , oddColors :: C.TextSpec }

colors :: B.PostingInfo -> BaseColors -> C.TextSpec
colors p b = let n = B.unVisibleNum . B.visibleNum $ p in
  if odd n then oddColors b else evenColors b

drCrToBaseColors :: Bits.DrCr -> DrCrColors -> BaseColors
drCrToBaseColors dc col = case dc of
  Bits.Debit -> BaseColors (evenDebit col) (oddDebit col)
  Bits.Credit -> BaseColors (evenCredit col) (oddCredit col)

noughtToBaseColors :: DrCrColors -> Bal.Nought -> BaseColors
noughtToBaseColors col no = case no of
  Bal.Zero -> BaseColors (evenZero col) (oddZero col)
  Bal.NonZero column -> drCrToBaseColors (Bal.drCr column) col
  
