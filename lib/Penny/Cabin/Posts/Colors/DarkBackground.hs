-- | The default colors; the user may override.
module Penny.Cabin.Posts.Colors.DarkBackground where

import qualified Penny.Cabin.Posts.Colors as PC
import qualified Penny.Cabin.Colors as CC
import qualified Penny.Cabin.Colors.Switch as S

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
evenTextSpec = CC.defaultSpec

oddTextSpec :: CC.TextSpec
oddTextSpec = S.switchBackground CC.defaultColor
              (CC.color256 235) evenTextSpec
              
-- | Debits in 256 colors are orange; in 8 colors, magenta
debit :: CC.TextSpec -> CC.TextSpec
debit = S.switchForeground CC.magenta (CC.color256 208)

-- | Credits in 256 colors are cyan; in 8 colors, cyan
credit :: CC.TextSpec -> CC.TextSpec
credit = S.switchForeground CC.cyan (CC.color256 45)

-- | Zero values are white
zero :: CC.TextSpec -> CC.TextSpec
zero = S.switchForeground CC.white (CC.color256 15)


