-- | The default colors; the user may override.
module Penny.Cabin.Colors.DarkBackground where

import qualified Penny.Cabin.Colors as PC
import qualified Penny.Cabin.Chunk as CC
import qualified Penny.Cabin.Chunk.Switch as S

baseColors :: PC.BaseColors
baseColors = PC.BaseColors evenTextSpec oddTextSpec

drCrColors :: PC.DrCrColors
drCrColors = PC.DrCrColors { PC.evenDebit = debit evenTextSpec
                           , PC.evenCredit = credit evenTextSpec
                           , PC.oddDebit = debit oddTextSpec
                           , PC.oddCredit = credit oddTextSpec
                           , PC.evenZero = zero evenTextSpec
                           , PC.oddZero = zero oddTextSpec }

evenTextSpec :: CC.TextSpec
evenTextSpec = CC.defaultTextSpec

oddTextSpec :: CC.TextSpec
oddTextSpec = S.switchBackground CC.color8_b_default
              CC.color256_b_235 evenTextSpec
              
-- | Debits in 256 colors are orange; in 8 colors, magenta
debit :: CC.TextSpec -> CC.TextSpec
debit = S.switchForeground CC.color8_f_magenta CC.color256_f_208

-- | Credits in 256 colors are cyan; in 8 colors, cyan
credit :: CC.TextSpec -> CC.TextSpec
credit = S.switchForeground CC.color8_f_cyan (CC.color256_f_45)

-- | Zero values are white
zero :: CC.TextSpec -> CC.TextSpec
zero = S.switchForeground CC.color8_f_white (CC.color256_f_15)


